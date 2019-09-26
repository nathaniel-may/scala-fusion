package Thrist

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

object PThrist {
  def compose[Arr[_, _], A, B](z: Arr[A, B], build:((Arr[C, D], Arr[D,E]) => Arr[C, E]) forSome { type C; type D; type E}, thrist: PThrist[Arr, A, B]): Arr[A, B] = thrist match {
    case PNil(_)           => z
    case PCons(head, tail) => build(head, compose(z, build, tail))
  }
}
