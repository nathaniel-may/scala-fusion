package Fusion

import Thrist.{PThrist, PNil, PCons, Category}

object Fused {

  // TODO just go straight to fuser
  implicit class Fusable[A](list: LazyList[A]) {
    def startFusion: Fuser[A, A] =
      new Fuser[A, A](PNil[Op, A](), toCoLazyList(list))
  }

  def map[A, B](f: A => B): Op[A, B] =
    cll => mkCoLazyList[B, cll.S](
      s => cll.next(s) match {
        case Done()          => Done()
        case Skip(sNext)     => Skip(sNext)
        case Yield(a, sNext) => Yield(f(a), sNext)
      },
      cll.state
    )

  def filter[A](f: A => Boolean): Op[A, A] =
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

  def mkCoLazyList[A, S0](n: S0 => Step[A, S0], s: S0): CoLazyList[A] {type S = S0} =
    new CoLazyList[A] {
      type S = S0
      override val next = n
      override val state = s
    }

  private[Fusion] def toCoLazyList[A](list: LazyList[A]): CoLazyList[A] =
    mkCoLazyList[A, LazyList[A]](
      {
        case LazyList() => Done()
        case x #:: xs   => Yield(x, xs)
      },
      list
    )

  private[Fusion] def toLazyList[A](co: CoLazyList[A]): LazyList[A] = {
    def go(s: co.S, n: co.S => Step[A, co.S]): LazyList[A] = n(s) match {
      case Done()          => LazyList.empty
      case Skip(sNext)     => go(sNext, n)
      case Yield(a, sNext) => a #:: go(sNext, n)
    }

    go(co.state, co.next)
  }

  type Op[A, B] = CoLazyList[A] => CoLazyList[B]
  implicit val opCategory: Category[Op] = new Category[Op]{
    override def id[A]: Op[A, A] = identity
    override def compose[A, B, C](f: Op[B, C], g: Op[A, B]): Op[A, C] = f compose g
  }

  // emulates GHC rewrite rules which are unavailable in Scalac
  private[Fusion] case class Fuser[A, B] (ops: PThrist[Op, A, B], state: CoLazyList[A]) {
    def fuse: LazyList[B] =
      toLazyList(PThrist.compose[Op, A, B](ops)(opCategory)(state))

    private[Fusion] def prepend[C](op: Op[B, C]): Fuser[A, C] =
      new Fuser[A, C](PCons[Op, A, B, C](op, ops), state)

    // user-visible functions //

    def map[C](f: B => C): Fuser[A, C] =
      Fuser(PCons[Op, A, B, C](Fused.map(f), ops), state)

    def filter(f: B => Boolean): Fuser[A, B] =
      Fuser(PCons[Op, A, B, B](Fused.filter(f), ops), state)
  }

}