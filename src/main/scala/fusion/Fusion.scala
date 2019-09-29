package fusion

import thrist.{Category, Cons, Nil, Thrist}

import scala.annotation.tailrec

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
  private[fusion] case class Fuser[X, A](ops: Thrist[Op, X, A], state: Stream[X]) {
    private[Fuser] def toStream =
      Thrist.compose[Op, X, A](ops)(opCategory)(state)

    def toLazyList: LazyList[A] =
      unstream(toStream)

    def toList: List[A] =
      toLazyList.toList

    def toVector: Vector[A] =
      toLazyList.toVector

    def map[B](f: A => B): Fuser[X, B] = {
      val map: Op[A, B] =
        cll => mkStream[B, cll.S](
          s => cll.next(s) match {
            case Done()          => Done()
            case Skip(sNext)     => Skip(sNext)
            case Yield(a, sNext) => Yield(f(a), sNext)
          },
          cll.state
        )

      Fuser(Cons[Op, X, A, B](map, ops), state)
    }

    def filter(f: A => Boolean): Fuser[X, A] = {
      val filter: Op[A, A] =
        cll => mkStream[A, cll.S](
          s => cll.next(s) match {
            case Done()                   => Done()
            case Skip(sNext)              => Skip(sNext)
            case Yield(a, sNext) if !f(a) => Skip(sNext)
            case Yield(a, sNext)          => Yield(a, sNext)
          },
          cll.state
        )

      Fuser(Cons[Op, X, A, A](filter, ops), state)
    }

    def take(n: Int): Fuser[X, A] = {
      val take: Op[A, A] =
        cll => mkStream[A, (Int, cll.S)](
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

      Fuser(Cons[Op, X, A, A](take, ops), state)
    }

    def foldLeft[B](z: B)(f: (B, A) => B): B = {
      val foldLeft: Stream[A] => B =
        stream => {
          @tailrec
          def go(z: B, s0: stream.S): B = stream.next(s0) match {
            case Done()          => z
            case Skip(sNext)     => go(z, sNext)
            case Yield(a, sNext) => go(f(z, a), sNext)
          }

          go(z, stream.state)
        }

      foldLeft(toStream)
    }

  }

}