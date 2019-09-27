package Fusion

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Fused._


object FusionProperties extends Properties("Fusion"){

  property("stream and unstream compose to the identity") = forAll {
    lazyList: LazyList[Int] =>
      lazyList == toLazyList(toCoLazyList(lazyList))
  }

  property("map is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.map(_ + 1) == lazyList.startFusion.map(_ + 1).fuse
  }

  property("filter is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.filter(_ > 0) == lazyList.startFusion.filter(_ > 0).fuse
  }

  property("filter then map is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.filter(_ > 0).map(_.toString) == lazyList.startFusion.filter(_ > 0).map(_.toString).fuse
  }

  property("take is the same") = forAll {
    (lazyList: LazyList[Int], n: Int) =>
      lazyList.take(n) == lazyList.startFusion.take(n).fuse
  }

}
