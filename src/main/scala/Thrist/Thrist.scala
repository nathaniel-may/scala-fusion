package Thrist

import scala.languageFeature.higherKinds
import scala.languageFeature.implicitConversions
import scala.languageFeature.existentials

//Thrist of functions
sealed trait FThrist[A,B]
case class FNil[A,B](implicit proof: A =:= B) extends FThrist[A,B]
case class FCons[A,B,C](head: A => B, tail: FThrist[B,C]) extends FThrist[A,C]

//Polymorphic Thrist
sealed trait PThrist[Arr[_, _], A, B]
case class PNil[Arr[_, _], A, B](implicit proof: A =:= B) extends PThrist[Arr, A, B]
case class PCons[Arr[_, _], A, B, C](head: Arr[B, C], tail: PThrist[Arr, A, B]) extends PThrist[Arr, A, C]

//Kind-polymorphic Thrist
sealed trait KThrist[Arr[_[_],_[_]], A[_], B[_]]
case class KNil[Arr[_[_],_[_]], A[_], B[_]](implicit proof: A =:= B) extends KThrist[Arr, A, B]
case class KCons[Arr[_[_],_[_]], A[_], B[_], C[_]](head: Arr[A, B], tail: KThrist[Arr, B, C]) extends KThrist[Arr, A, C]

sealed trait Category[X[_, _]] {
  type A
  type B
  type C
  val id: X[A, B]
  def compose(a: X[A, B], b: X[B, C]): X[A, C]
}

object PThrist {

  def compose[Arr[_, _], A, B](thrist: PThrist[Arr, A, B])(implicit ev: Category[Arr]): Arr[A, B] = thrist match {
    case PNil(_)           => ev.id
    case PCons(head, tail) => ev.compose(head, compose(tail)) {type B0 = B}
  }

//  def compose[Arr[_, _], A, B](z: Arr[_, _], build:((Arr[C, D], Arr[D,E]) => Arr[C, E]) forSome { type C; type D; type E}, thrist: PThrist[Arr, A, B]): Arr[_, _] = thrist match {
//    case PNil(_)           => z
//    case PCons(head, tail) => build((head, compose(z, build, tail)))
//  }

}
