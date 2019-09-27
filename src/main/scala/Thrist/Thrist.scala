package Thrist

import scala.language.{implicitConversions, existentials}

//Thrist of functions
sealed trait FThrist[A,B]
//case class FNil[A,B](implicit proof: A =:= B) extends FThrist[A,B]
case class FNil[A]() extends FThrist[A,A]
case class FCons[A,B,C](head: A => B, tail: FThrist[B,C]) extends FThrist[A,C]

//Polymorphic Thrist
sealed trait PThrist[Arr[_, _], A, B]
//case class PNil[Arr[_, _], A, B](implicit proof: A =:= B) extends PThrist[Arr, A, B]
case class PNil[Arr[_, _], A]() extends PThrist[Arr, A, A]
case class PCons[Arr[_, _], A, B, C](head: Arr[B, C], tail: PThrist[Arr, A, B]) extends PThrist[Arr, A, C]

//Kind-polymorphic Thrist
sealed trait KThrist[Arr[_[_],_[_]], A[_], B[_]]
case class KNil[Arr[_[_],_[_]], A[_]]() extends KThrist[Arr, A, A]
case class KCons[Arr[_[_],_[_]], A[_], B[_], C[_]](head: Arr[A, B], tail: KThrist[Arr, B, C]) extends KThrist[Arr, A, C]

trait Category[Hom[_, _]] {
  def id[A]: Hom[A, A]
  def compose[A, B, C](a: Hom[A, B], b: Hom[B, C]): Hom[A, C]
}

object PThrist {

  def compose[Arr[_, _], A, B](thrist: PThrist[Arr, A, B])(implicit cat: Category[Arr]): Arr[A, B] = thrist match {
    case _: PNil[_, a]     => cat.id[a] // TODO better way to do this?
    case PCons(head, tail) => cat.compose(compose(tail), head)
  }

}
