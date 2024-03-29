package fusion

import org.scalacheck.{Arbitrary, Gen}, Gen.oneOf
import Fusion._
import fusion.syntax._

object Generators {
  
  def genFusion(implicit ev: Arbitrary[LazyList[Int]]): Gen[LazyList[Int]] =
    Gen.sized { size =>
      Gen.resize(size, ev.arbitrary).flatMap { ll =>
        addMethodCalls(size, ll.fuse)(implicitly[Arbitrary[Int]])
      }.map(_.toLazyList)
    }

  def addMethodCalls(n: Int, fuser: Fuser[Int, Int])(ev: Arbitrary[Int]): Gen[Fuser[Int, Int]] =
    if (n <= 0) Gen.const(fuser)
    else addMethodCall(fuser)(ev).flatMap { fNext => addMethodCalls(n-1, fNext)(ev) }

  private def addMethodCall(fuser: Fuser[Int, Int])(ev: Arbitrary[Int]): Gen[Fuser[Int, Int]] =
    oneOf(1 to 4).flatMap {
      case 1 => ev.arbitrary.map { n => fuser.map(_ + n) }
      case 2 => ev.arbitrary.map { n => fuser.filter(_ < n) }
      case 3 => ev.arbitrary.map { n => fuser.take(n) }
      case 4 => ev.arbitrary.map { _ => fuser.flatMap { x => LazyList(x, x) } }
    }

}
