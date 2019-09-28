package consumer

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import fusion.syntax._

// ensures library can be used as intended outside the package scope
object ConsumerProperties extends Properties("Fusion consumer") {
  property("map is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.map(_ + 1) == lazyList.startFusion.map(_ + 1).fuse
  }
}
