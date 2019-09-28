package fusion

package object syntax {
  import Fusion._
  import thrist.Nil

  implicit class Fusable[A](list: LazyList[A]) {
    def startFusion: Fuser[A, A] =
      new Fuser[A, A](Nil[Op, A](), stream(list))
  }
}
