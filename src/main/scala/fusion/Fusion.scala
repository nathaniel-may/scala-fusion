package fusion

import thrist.{Category, Cons, Thrist}

import scala.annotation.tailrec
import scala.{Stream => _}
import scala.collection.{SeqFactory => _}

/** Container for fusion types and instances */
object Fusion {

  /** Steps as defined in the paper
    *
    * @tparam A - the type of any yielded value from the step
    * @tparam S - state of the step itself
    */
  private[fusion] sealed trait Step[A, S]
  private[fusion] case class Done[A, S]() extends Step[A, S]
  private[fusion] case class Skip[A, S](s: S) extends Step[A, S]
  private[fusion] case class Yield[A, S](a: A, s: S) extends Step[A, S]

  /** The Stream data type as defined in the paper which is unrelated to the
    * predecessor to Scala's LazyList.
    *
    * Streams are "non-recursive co-structures" where Lists are recursive structures.
    * They use a similar mechanic to `unfold`.
    */
  private[fusion] sealed trait Stream[A] {
    type S
    val next: S => Step[A, S]
    val state: S
  }

  /** Constructor for Stream type */
  private[fusion] def mkStream[A, S0](n: S0 => Step[A, S0], s: S0): Stream[A] {type S = S0} =
    new Stream[A] {
      type S = S0
      override val next = n
      override val state = s
    }

  /** Constructor for Stream type from an existing LazyList
    *
    * note: The stream is not accessed on conversion. It simply becomes the state itself.
    */
  private[fusion] def stream[A](list: LazyList[A]): Stream[A] =
    mkStream[A, LazyList[A]](
      {
        case LazyList() => Done()
        case x #:: xs   => Yield(x, xs)
      },
      list
    )

  /** Converts a Stream to a LazyList by evaluating the built up instruction set */
  private[fusion] def unstream[A](co: Stream[A]): LazyList[A] = {
    def go(s: co.S, n: co.S => Step[A, co.S]): LazyList[A] = n(s) match {
      case Done()          => LazyList.empty
      case Skip(sNext)     => go(sNext, n)
      case Yield(a, sNext) => a #:: go(sNext, n)
    }

    go(co.state, co.next)
  }

  private[fusion] type Op[A, B] = Stream[A] => Stream[B]

  /** Type class instance used for evaluation of the Fuser's Thrist */
  private[fusion] implicit val opCategory: Category[Op] = new Category[Op]{
    override def id[A]: Op[A, A] = identity
    override def compose[A, B, C](f: Op[B, C], g: Op[A, B]): Op[A, C] = f compose g
  }

  /** The Fuser type emulates GHC rewrite rules which are unavailable in Scalac.
   *
   * This type is exposed to the user and thus should contain all the expected methods.
   */
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

    def foldRight[B](z: B)(f: (A, B) => B): B = {
      val foldRight: Stream[A] => B =
        stream => {
          def go(s: stream.S): B = stream.next(s) match {
            case Done()          => z
            case Skip(sNext)     => go(sNext)
            case Yield(a, sNext) => f(a, go(sNext))
          }

          go(stream.state)
        }

      foldRight(toStream)
    }

    def flatMap[B](f: A => LazyList[B]) =
      flatMap0(a => stream(f(a)))

    private[fusion] def flatMap0[B](f: A => Stream[B]) = {
      val flatMap: Op[A, B] =
        cll => mkStream[B, (cll.S, Option[Stream[B]])](
          {
            case (sa, None) => cll.next(sa) match {
              case Done()          => Done()
              case Skip(sNext)     => Skip((sNext, None))
              case Yield(a, sNext) => Skip((sNext, Some(f(a))))
            }
            case (sa, Some(bs)) => bs.next(bs.state) match {
              case Done()          => Skip((sa, None))
              case Skip(sNext)     => Skip((sa, Some(mkStream(bs.next, sNext))))
              case Yield(b, sNext) => Yield(b, (sa, Some(mkStream(bs.next, sNext))))
            }
          },
          (cll.state, None)
        )

      Fuser(Cons[Op, X, A, B](flatMap, ops), state)
    }

  }

}