package thrist

import thrist.Thrist.{FoldLeftFn, FoldRightFn}
import scala.annotation.tailrec
import scala.language.{existentials, implicitConversions}


case class Nil[Arr[_, _], A, B](ev: A =:= B) extends Thrist[Arr, A, B]
case class Cons[Arr[_, _], A, B, C](head: Arr[A, B], tail: Thrist[Arr, B, C]) extends Thrist[Arr, A, C]

/** Thrist is a "type-threaded list"
 * based on the haskell package `Thrist`:
 * http://hackage.haskell.org/package/thrist
 */
sealed trait Thrist[Arr[_, _], A, B] {
  final def foldRight[Brr[_,_], X](z: Brr[X, A])(f: FoldRightFn[Arr, Brr, B]): Brr[X, B] = {
    type Brr0[Z] = Brr[X, Z]
    this match {
      case Nil(ev)                  => ev.substituteCo[Brr0](z)
      case cons: Cons[Arr, A, x, B] => f.fn[x, B](cons.head, cons.tail.foldRight[Brr, X](z)(new FoldRightFn[Arr, Brr, x]{
        def fn[I, J](ij: Arr[I, J], jc: Brr[J, x]): Brr[I, x] = f.fn(ij, jc)
      }))
    }
  }

  @tailrec
  final def foldLeft[Brr[_,_], X](z: Brr[X, A])(f: FoldLeftFn[Arr, Brr, X]): Brr[X, B] = {
    type Brr0[Z] = Brr[X, Z]
    this match {
      case Nil(ev)                  => ev.substituteCo[Brr0](z)
      case cons: Cons[Arr, A, x, B] => cons.tail.foldLeft(f.fn[A, B](z, cons.head))(f)
    }
  }
}

/** Custom Category typeclass instead of depending on cats or scalaz
  * @tparam Hom - Homomorphism over two types
  */
trait Category[Hom[_, _]] {
  def id[A]: Hom[A, A]
  def compose[A, B, C](f: Hom[B, C], g: Hom[A, B]): Hom[A, C]
}

object Thrist {
  trait FoldLeftFn[Arr[_, _], Brr[_, _], X] {
    def fn[J,K](aj: Brr[X, J], jk: Arr[J, K]): Brr[X, K]
  }

  trait FoldRightFn[Arr[_, _], Brr[_, _], B] {
    def fn[I, J](ij: Arr[I, J], jc: Brr[J, B]): Brr[I, B]
  }

  def compose[Arr[_, _], A, B](thrist: Thrist[Arr, A, B])(implicit cat: Category[Arr]): Arr[A, B] =
    thrist.foldLeft(cat.id) {
      new FoldLeftFn[Arr, Arr, A] {
        def fn[J,K](aj: Arr[A, J], jk: Arr[J, K]): Arr[A, K] =
          cat.compose(aj, jk)
      }
    }
}
