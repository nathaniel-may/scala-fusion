package fusion

package object syntax {
  import Fusion._
  import thrist.Nil

  implicit class Fusable[A](list: LazyList[A]) {
    def fuse: Fuser[A, A] =
      new Fuser[A, A](Nil[Op, A, A](implicitly[A =:= A]), stream(list))
  }
}
