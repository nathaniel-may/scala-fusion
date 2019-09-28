package fusion

import thrist.{Thrist, Nil, Cons, Category}

object Fusion {

  private[fusion] sealed trait Step[A, S]
  private[fusion] case class Done[A, S]() extends Step[A, S]
  private[fusion] case class Skip[A, S](s: S) extends Step[A, S]
  private[fusion] case class Yield[A, S](a: A, s: S) extends Step[A, S]

  private[fusion] sealed trait Stream[A] {
    type S
    val next: S => Step[A, S]
    val state: S
  }

  private[fusion] def mkStream[A, S0](n: S0 => Step[A, S0], s: S0): Stream[A] {type S = S0} =
    new Stream[A] {
      type S = S0
      override val next = n
      override val state = s
    }

  private[fusion] def stream[A](list: LazyList[A]): Stream[A] =
    mkStream[A, LazyList[A]](
      {
        case LazyList() => Done()
        case x #:: xs   => Yield(x, xs)
      },
      list
    )

  private[fusion] def unstream[A](co: Stream[A]): LazyList[A] = {
    def go(s: co.S, n: co.S => Step[A, co.S]): LazyList[A] = n(s) match {
      case Done()          => LazyList.empty
      case Skip(sNext)     => go(sNext, n)
      case Yield(a, sNext) => a #:: go(sNext, n)
    }

    go(co.state, co.next)
  }

  private[fusion] type Op[A, B] = Stream[A] => Stream[B]
  private[fusion] implicit val opCategory: Category[Op] = new Category[Op]{
    override def id[A]: Op[A, A] = identity
    override def compose[A, B, C](f: Op[B, C], g: Op[A, B]): Op[A, C] = f compose g
  }

  // emulates GHC rewrite rules which are unavailable in Scalac
  private[fusion] case class Fuser[A, B](ops: Thrist[Op, A, B], state: Stream[A]) {
    def fuse: LazyList[B] =
      unstream(Thrist.compose[Op, A, B](ops)(opCategory)(state))

    def map[C](f: B => C): Fuser[A, C] = {
      val map: Op[B, C] =
        cll => mkStream[C, cll.S](
          s => cll.next(s) match {
            case Done()          => Done()
            case Skip(sNext)     => Skip(sNext)
            case Yield(a, sNext) => Yield(f(a), sNext)
          },
          cll.state
        )

      Fuser(Cons[Op, A, B, C](map, ops), state)
    }

    def filter(f: B => Boolean): Fuser[A, B] = {
      val filter: Op[B, B] =
        cll => mkStream[B, cll.S](
          s => cll.next(s) match {
            case Done()                   => Done()
            case Skip(sNext)              => Skip(sNext)
            case Yield(a, sNext) if !f(a) => Skip(sNext)
            case Yield(a, sNext)          => Yield(a, sNext)
          },
          cll.state
        )

      Fuser(Cons[Op, A, B, B](filter, ops), state)
    }

    def take(n: Int): Fuser[A, B] = {
      val take: Op[B, B] =
        cll => mkStream[B, (Int, cll.S)](
          s => {
            val (n0, s0) = s
            if (n0 <= 0) Done()
            else cll.next(s0) match {
              case Done() => Done()
              case Skip(sNext) => Skip((n0, sNext))
              case Yield(a, sNext) => Yield(a, (n - 1, sNext))
            }
          },
          (n, cll.state)
        )

      Fuser(Cons[Op, A, B, B](take, ops), state)
    }
  }

}