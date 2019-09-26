package Fusion

import scala.languageFeature.higherKinds
import scala.languageFeature.implicitConversions
import scala.languageFeature.existentials
import Stream.Empty

object Fused {

  implicit class Fusable[A](list: Stream[A]) {
    def mapFused[B](f: A => B): Fuser[A, B] =
      new Fuser[A, B](Cons(Fused.mapFused[A, B](f), Nil[Op, A, A]), toCoLazyList(list))

    def filterFused(f: A => Boolean): Fuser[A, A] =
      new Fuser[A, A](Cons(Fused.filterFused[A](f), Nil[Op, A, A]), toCoLazyList(list))
  }

  implicit def unStream[A, B](fuser: Fuser[A, B]): Stream[B] = fuser.toStream
  implicit def unStream[A](fuser: Fuser[A, A]): Stream[A] = fuser.toStream

  def mapFused[A, B](f: A => B): Op[A, B] =
    cll =>
      CoLazyList[B](
        cll.state,
        (s: S forSome { type S }) => cll.next(s) match {
          case Done()          => Done()
          case Skip(sNext)     => Skip(sNext)
          case Yield(a, sNext) => Yield(f(a), sNext)
        }
      )

  def filterFused[A](f: A => Boolean): Op[A, A] =
    cll =>
      CoLazyList[A](
        cll.state,
        s => cll.next(s) match {
          case done@Done()              => done
          case skip@Skip(_)             => skip
          case Yield(a, sNext) if !f(a) => Skip(sNext)
          case yieldas@Yield(_, _)      => yieldas
        }
      )

  private[Fusion] sealed trait Step[A, S]
  private[Fusion] case class Done[A, S]() extends Step[A, S]
  private[Fusion] case class Skip[A, S](s: S) extends Step[A, S]
  private[Fusion] case class Yield[A, S](a: A, s: S) extends Step[A, S]

  private[Fusion] case class CoLazyList[A](all: (S, S => Step[A, S]) forSome { type S }) {
    val state = all._1
    val next = all._2
  }

  private[Fusion] def toCoLazyList[A](list: Stream[A]): CoLazyList[A] =
    CoLazyList(
      (list,
        (in: Stream[A]) => {
          case Empty    => Done()
          case x #:: xs => Yield(x, xs)
        })
    )

  private[Fusion] def toLazyList[A](co: CoLazyList[A]): Stream[A] = {
    def go(all: (S, S => Step[A, S]) forSome { type S }): Stream[A] = all._2(all._1) match {
      case Done()          => Stream.empty
      case Skip(sNext)     => go(sNext, all._2)
      case Yield(a, sNext) => a #:: go(sNext, all._2)
    }

    go(co.state, co.next)
  }

  type Op[A, B] = CoLazyList[A] => CoLazyList[B]
  type Ops[A, B] = Thrist[Op, A, B]

  // emulates GHC rewrite rules which are unavailable in Scalac
  private[Fusion] case class Fuser[A, B] (ops: Ops[A, B], state: CoLazyList[A]) {
    private[Fusion] def toStream: Stream[B] =
      Thrist.compose[Op, A, B](_ => state, (a: Op[_, A], b: Op[A, _]) => b compose a, ops)(state)

    private[Fusion] def prepend[C](op: Op[B, C]): Fuser[A, C] =
      new Fuser[A, C](Cons[Op, A, B, C](op, ops), state)

    // user-visible functions //

    def mapFused[C](f: B => C): Fuser[A, C] =
      Fuser(Cons[Op, A, B, C](Fused.mapFused(f), ops), state)

    def filterFused(f: A => Boolean): Fuser[A, B] =
      Fuser(Cons[Op, A, B, B](Fused.filterFused(f), ops), state)
  }

}

//Thrist of functions
//sealed trait Thrist[A,B]
//case class Nil[A,B](implicit proof: A =:= B) extends Thrist[A,B]
//case class Cons[A,B,C](head: A => B, tail: Thrist[B,C]) extends Thrist[A,C]


//Polymorphic Thrist (diagramatic order)
//sealed trait Thrist[Arr[_, _], A, B]
//case class Nil[Arr[_, _], A, B](implicit proof: A =:= B) extends Thrist[Arr, A, B]
//case class Cons[Arr[_, _], A, B, C](head: Arr[A, B], tail: Thrist[Arr, B, C]) extends Thrist[Arr, A, C]

//Polymorphic Thrist
sealed trait Thrist[Arr[_, _], A, B]
case class Nil[Arr[_, _], A, B](implicit proof: A =:= B) extends Thrist[Arr, A, B]
case class Cons[Arr[_, _], A, B, C](head: Arr[B, C], tail: Thrist[Arr, A, B]) extends Thrist[Arr, A, C]

//Kind-polymorphic Thrist
//sealed trait Thrist[Arr[_[_],_[_]], A[_], B[_]]
//case class Nil[Arr[_[_],_[_]], A[_], B[_]](implicit proof: A =:= B) extends Thrist[Arr, A, B]
//case class Cons[Arr[_[_],_[_]], A[_], B[_], C[_]](head: Arr[A, B], tail: Thrist[Arr, B, C]) extends Thrist[Arr, A, C]

object Thrist {
  def compose[Arr[_, _], A, B](z: Arr[A, B], build:((Arr[C, D], Arr[D,E]) => Arr[C, E]) forSome { type C; type D; type E}, thrist: Thrist[Arr, A, B]): Arr[A, B] = thrist match {
    case Nil(_)           => z
    case Cons(head, tail) => build(head, compose(z, build, tail))
  }
}