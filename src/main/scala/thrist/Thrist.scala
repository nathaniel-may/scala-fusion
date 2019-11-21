package thrist

import scala.language.{implicitConversions, existentials}

/** Thrist is a "type-threaded list"
  * based on the haskell package `Thrist`:
  * http://hackage.haskell.org/package/thrist
  */
sealed trait Thrist[Arr[_, _], A, B]
case class Nil[Arr[_, _], A]() extends Thrist[Arr, A, A]
case class Cons[Arr[_, _], A, B, C](head: Arr[B, C], tail: Thrist[Arr, A, B]) extends Thrist[Arr, A, C]

/** Custom Category typeclass instead of depending on cats or scalaz
  * @tparam Hom - Homomorphism over two types
  */
trait Category[Hom[_, _]] {
  def id[A]: Hom[A, A]
  def compose[A, B, C](f: Hom[B, C], g: Hom[A, B]): Hom[A, C]
}

object Thrist {
  def compose[Arr[_, _], A, B](thrist: Thrist[Arr, A, B])(implicit cat: Category[Arr]): Arr[A, B] =
    thrist match {
      case _: Nil[_, a]     => cat.id[a]
      case Cons(head, tail) => cat.compose(head, compose(tail))
    }
}
