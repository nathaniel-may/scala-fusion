package fusion

import org.scalatest._
import org.scalacheck.Gen
import scala.math.abs
import Generators.genFusion
import fusion.syntax._

class GeneratorTest extends FlatSpec with Matchers {

  "A fusion generator" should "generate different fusions" in {
    val size      = 1
    val samples   = 200
    val input: LazyList[LazyList[Int]] =
      sequence(LazyList.fill(samples)(Gen.resize(size, genFusion).sample))
        .to(LazyList).flatten

    val map = input.foldLeft[Map[List[Int], Int]](Map()) {
        (m, ll) =>
          val k = ll.toList
          m.updated(k, m.getOrElse(k, 0) + 1)
    }

    val mostCommon = map.valuesIterator.max
    val allButMostCommon = map.valuesIterator.filterNot(_ == mostCommon).sum

    withClue(s"Out of $samples samples, the most common fusion occurred $mostCommon times, all other fusions occured $allButMostCommon times") {
      allButMostCommon > 0 shouldBe true }
  }

  // TODO get cats sequence to work with LazyList
  def sequence[A]: LazyList[Option[A]] => Option[LazyList[A]] = {
    case LazyList()       => Option(LazyList())
    case None    #:: tail => None
    case Some(x) #:: tail => sequence(tail).map { x #:: _}
  }

}
