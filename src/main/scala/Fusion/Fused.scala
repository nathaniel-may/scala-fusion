package Fusion

import scala.languageFeature.higherKinds
import scala.languageFeature.implicitConversions
import Stream.Empty

object Fused {

  implicit class Fusable[A](list: Stream[A]) {
    def mapFused[B](f: A => B): Fuser[A, B] =
      Fuser(Fused.mapFused[A, B](f), toCoLazyList(list))

    def filterFused(f: A => Boolean): Fuser[A, A] =
      Fuser(Fused.filterFused[A](f), toCoLazyList(list))
  }

  implicit def unStream[A, B](fuser: Fuser[A, B]): Stream[B] = fuser.toStream
  implicit def unStream[A](fuser: Fuser[A, A]): Stream[A] = fuser.toStream

  def mapFused[A, B](f: A => B): Op[A, B] =
    cll =>
      CoLazyList[B, Stream[A]](
        cll.state,
        s => cll.next(s) match {
          case Done()          => Done()
          case Skip(sNext)     => Skip(sNext)
          case Yield(a, sNext) => Yield(f(a), sNext)
        }
      )

  def filterFused[A](f: A => Boolean): Op[A, A] =
    cll =>
      CoLazyList[A, Stream[A]](
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

  private[Fusion] case class CoLazyList[A, S](state: S, next: S => Step[A, S])

  private[Fusion] def toCoLazyList[A](list: Stream[A]): CoLazyList[A, Stream[A]] =
    CoLazyList(
      list,
      {
        case Empty    => Done()
        case x #:: xs => Yield(x, xs)
      }
    )

  private[Fusion] def toLazyList[A, S](co: CoLazyList[A, S]): Stream[A] = {
    def go(s: S, next: S => Step[A, S]): Stream[A] = next(s) match {
      case Done()          => Stream.empty
      case Skip(sNext)     => go(sNext, next)
      case Yield(a, sNext) => a #:: go(sNext, next)
    }

    go(co.state, co.next)
  }

  type CLL[A, B] = CoLazyList[B, Stream[A]]
  type Op[A, B] = CLL[A, A] => CLL[A, B]
  type Ops[A, B] = Thrist[Op, A, B]

  // emulates GHC rewrite rules which are unavailable in Scala
  private[Fusion] case class Fuser[A, B] (ops: Ops[A, B], state: CLL[A, A]) {
    implicit val composableOp = new Monoid[Fused.Op] {
      override def id[C]: Op[C, C] = identity
      override def compose[C, D, E](a: Op[C, D], b: Op[D, E]): Op[C, E] =
        b.compose[CLL[C, D]](a)
    }

    private[Fusion] def toStream: Stream[B] =
      Thrist.compose[Op, A, B]((a, b) => CoLazyList.apply(b, a), state, ops)(state)

    private[Fusion] def prepend[C](op: Op[B, C]): Fuser[A, C] =
      new Fuser[A, C](Cons[Op, A, B, C](op, ops), state)

    // user-visible functions //

    def mapFused[C](f: B => C): Fuser[A, C] =
      Fuser(Cons[Op, A, B, C](Fused.mapFused(f), ops), state)

    def filterFused(f: A => Boolean): Fuser[A, B] =
      Fuser(Cons[Op, A, B, B](Fused.filterFused(f), ops), state)
  }

  private[Fusion] object Fuser {
    def apply[A, B](op: Op[A, B], s: CLL[A, A]) = new Fuser[A, B](Cons(op, Nil[CLL, A, A]), s)
  }

}

// type class TODO scalaz? cats?
trait Monoid[M[_, _]] {
  def id[A]: M[A, A]
  def compose[A, B, C](a: M[A, B], b: M[B, C]): M[A, C]
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
  def compose[Arr[_, _], A, B, C](thrist: Thrist[Arr, A, B])(implicit ev: Monoid[Arr]): Arr[A, B] = thrist match {
    case Nil(_)           => default
    case Cons(head, tail) => ev.compose(head, compose(tail))
  }
}