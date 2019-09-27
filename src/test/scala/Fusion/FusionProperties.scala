package Fusion

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Fused._


object FusionProperties extends Properties("Fusion"){

  property("stream and unstream compose to the identity") = forAll {
    lazyList: LazyList[Int] =>
      lazyList == toLazyList(toCoLazyList(lazyList))
  }

  property("mapFused is the same as map") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.map(_ + 1) == lazyList.startFusion.map(_ + 1).fuse
  }

  property("filterFused is the same as filter") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.filter(_ > 0) == lazyList.startFusion.filter(_ > 0).fuse
  }

  property("filterFused then mapFused is the same as filter then map") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.filter(_ > 0).map(_.toString) == lazyList.startFusion.filter(_ > 0).map(_.toString).fuse
  }

}
