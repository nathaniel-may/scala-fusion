package fusion

import org.scalacheck.{Properties, Arbitrary}
import org.scalacheck.Prop.forAll
import Generators.genFusion
import Fused._


object FusionProperties extends Properties("Fusion"){

  property("stream and unstream compose to the identity") = forAll {
    lazyList: LazyList[Int] =>
      lazyList == unstream(stream(lazyList))
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

  property("fusing take at the end reduces the number of computations") = forAll {
    (lazyList: LazyList[Int], n: Int)  =>
      var noFusionEffects = 0
      var fusionEffects = 0

      // mutations to simulate effects
      // comparing with list effects because lazylist has its own optimizations
      lazyList.toList.map(_ => noFusionEffects += 1).take(n)
      lazyList.startFusion.map(_ => fusionEffects += 1).take(n).fuse.toList

      if(lazyList.drop(n).nonEmpty) fusionEffects < noFusionEffects
      else fusionEffects == noFusionEffects
  }

  property("an arbitrary fusion is never worse than using SeqView or native LazyList optimizations") =
    forAll(genFusion, implicitly[Arbitrary[Int]].arbitrary) {
      (lazyList: LazyList[Int], n: Int)  =>
        var lazyListEffects = 0
        var seqViewEffects = 0
        var fusionEffects = 0

        // mutations to simulate effects
        lazyList.map(_ => lazyListEffects += 1).take(n).toList
        lazyList.view.map(_ => seqViewEffects += 1).take(n).toList
        lazyList.startFusion.map(_ => fusionEffects += 1).take(n).fuse.toList

        List(lazyListEffects, seqViewEffects)
          .forall(fusionEffects <= _)
    }

}