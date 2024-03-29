package fusion

import org.scalacheck.{Properties, Arbitrary}
import org.scalacheck.Prop.forAll
import Generators.genFusion
import fusion.syntax._
import Fusion._

object FusionProperties extends Properties("Fusion"){

  property("stream and unstream compose to the identity") = forAll {
    lazyList: LazyList[Int] =>
      lazyList == unstream(stream(lazyList))
  }

  property("map is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.map(_ + 1) == lazyList.fuse.map(_ + 1).toLazyList
  }

  property("filter is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.filter(_ > 0) == lazyList.fuse.filter(_ > 0).toLazyList
  }

  property("filter then map is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.filter(_ > 0).map(_.toString) == lazyList.fuse.filter(_ > 0).map(_.toString).toLazyList
  }

  property("filter then filter is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.filter(_ > 0).filter(_ < 100) == lazyList.fuse.filter(_ > 0).filter(_ < 100).toLazyList
  }

  property("take is the same") = forAll {
    (lazyList: LazyList[Int], n: Int) =>
      lazyList.take(n) == lazyList.fuse.take(n).toLazyList
  }

  property("take ten take is the same") = forAll {
    (lazyList: LazyList[Int], n: Int, n2: Int) =>
      lazyList.take(n).take(n2) == lazyList.fuse.take(n).take(n2).toLazyList
  }

  property("foldLeft is the same") = forAll {
    (lazyList: LazyList[Int], n: Int) =>
      lazyList.foldLeft("") { (s, n) => s + n.toString } ==
        lazyList.fuse.foldLeft("") { (s, n) => s + n.toString }
  }

  property("foldRight is the same") = forAll {
    (lazyList: LazyList[Int], n: Int) =>
      lazyList.foldRight("") { (n, s) => s + n.toString } ==
        lazyList.fuse.foldRight("") { (n, s) => s + n.toString }
  }

  property("take then foldLeft is the same") = forAll {
    (lazyList: LazyList[Int], n: Int) =>
      lazyList.take(n).foldLeft("") { (s, n) => s + n.toString } ==
        lazyList.fuse.take(n).foldLeft("") { (s, n) => s + n.toString }
  }

  property("take then foldRight is the same") = forAll {
    (lazyList: LazyList[Int], n: Int) =>
      lazyList.take(n).foldRight("") { (n, s) => s + n.toString } ==
        lazyList.fuse.take(n).foldRight("") { (n, s) => s + n.toString }
  }

  property("flatMap is the same") = forAll {
    lazyList: LazyList[Int] =>
      lazyList.flatMap { x => LazyList(x, x) } == lazyList.fuse.flatMap { x => LazyList(x, x) }.toLazyList
  }

  property("take then flatMap is the same") = forAll {
    (lazyList: LazyList[Int], n: Int) =>
      lazyList.take(n).flatMap { x => LazyList(x, x) } == lazyList.fuse.take(n).flatMap { x => LazyList(x, x) }.toLazyList
  }

  property("fusing take at the end reduces the number of computations") = forAll {
    (lazyList: LazyList[Int], n: Int)  =>
      var noFusionEffects = 0
      var fusionEffects = 0

      // mutations to simulate effects
      // comparing with list effects because lazylist has its own optimizations
      lazyList.toList.map(_ => noFusionEffects += 1).take(n)
      lazyList.fuse.map(_ => fusionEffects += 1).take(n).toList

      if(lazyList.drop(n).nonEmpty) fusionEffects < noFusionEffects
      else fusionEffects == noFusionEffects
  }

  property("fusing take at the end is never worse than using SeqView or native LazyList optimizations") = forAll {
    (lazyList: LazyList[Int], n: Int)  =>
      var lazyListEffects = 0
      var seqViewEffects = 0
      var fusionEffects = 0

      // mutations to simulate effects
      lazyList.map(_ => lazyListEffects += 1).take(n).toList
      lazyList.view.map(_ => seqViewEffects += 1).take(n).toList
      lazyList.fuse.map(_ => fusionEffects += 1).take(n).toList

      List(lazyListEffects, seqViewEffects)
        .forall(fusionEffects <= _)
  }

}
