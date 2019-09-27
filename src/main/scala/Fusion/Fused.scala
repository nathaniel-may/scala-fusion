package Fusion

import Stream.Empty

import Thrist.{PThrist, PNil, PCons}

object Fused {

  implicit class Fusable[A](list: Stream[A]) {
    def mapFused[B](f: A => B): Fuser[A, B] =
      new Fuser[A, B](PCons(Fused.mapFused[A, B](f), PNil[Op, A, A]), toCoLazyList(list))

    def filterFused(f: A => Boolean): Fuser[A, A] =
      new Fuser[A, A](PCons(Fused.filterFused[A](f), PNil[Op, A, A]), toCoLazyList(list))
  }

  implicit def unStream[A, B](fuser: Fuser[A, B]): Stream[B] = fuser.toStream
  implicit def unStream[A](fuser: Fuser[A, A]): Stream[A] = fuser.toStream

  def mapFused[A, B](f: A => B): Op[A, B] =
    cll => mkCoLazyList[B, cll.S](
      s => cll.next(s) match {
        case Done()          => Done()
        case Skip(sNext)     => Skip(sNext)
        case Yield(a, sNext) => Yield(f(a), sNext)
      },
      cll.state
    )

  def filterFused[A](f: A => Boolean): Op[A, A] =
    cll => mkCoLazyList[A, cll.S](
      s => cll.next(s) match {
        case Done()                   => Done()
        case Skip(sNext)              => Skip(sNext)
        case Yield(a, sNext) if !f(a) => Skip(sNext)
        case Yield(a, sNext)          => Yield(a, sNext)
      },
      cll.state
    )

  private[Fusion] sealed trait Step[A, S]
  private[Fusion] case class Done[A, S]() extends Step[A, S]
  private[Fusion] case class Skip[A, S](s: S) extends Step[A, S]
  private[Fusion] case class Yield[A, S](a: A, s: S) extends Step[A, S]


  private[Fusion] sealed trait CoLazyList[A] {
    type S
    val next: S => Step[A, S]
    val state: S
  }

  def mkCoLazyList[A, S0](next: S0 => Step[A, S0], state: S0): CoLazyList[A] {type S = S0} =
    new CoLazyList[A] {
      type S = S0
      override val next = next
      override val state = state
    }

  private[Fusion] def toCoLazyList[A](list: Stream[A]): CoLazyList[A] =
    mkCoLazyList[A, _](
      {
        case Empty    => Done()
        case x #:: xs => Yield(x, xs)
      },
      list
    )

  private[Fusion] def toLazyList[A](co: CoLazyList[A]): Stream[A] = {
    def go(s: co.S, n: co.S => Step[A, co.S]): Stream[A] = n(s) match {
      case Done()          => Stream.empty
      case Skip(sNext)     => go(sNext, n)
      case Yield(a, sNext) => a #:: go(sNext, n)
    }

    go(co.state, co.next)
  }

  type Op[A, B] = CoLazyList[A] => CoLazyList[B]

  // emulates GHC rewrite rules which are unavailable in Scalac
  private[Fusion] case class Fuser[A, B] (ops: PThrist[Op, A, B], state: CoLazyList[A]) {
    private[Fusion] def toStream: Stream[B] = ops match {
      case PNil(proof) => toLazyList(state)
      case PCons(h, t) => toLazyList(h(state))
    }
      //PThrist.compose[Op, A, B](_ => state, (a: Op[_, A], b: Op[A, _]) => b compose a, ops)(state)

    private[Fusion] def prepend[C](op: Op[B, C]): Fuser[A, C] =
      new Fuser[A, C](PCons[Op, A, B, C](op, ops), state)

    // user-visible functions //

    def mapFused[C](f: B => C): Fuser[A, C] =
      Fuser(PCons[Op, A, B, C](Fused.mapFused(f), ops), state)

    def filterFused(f: A => Boolean): Fuser[A, B] =
      Fuser(PCons[Op, A, B, B](Fused.filterFused(f), ops), state)
  }

}