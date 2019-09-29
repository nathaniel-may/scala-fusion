package consumer

import org.scalatest._
import fusion.syntax._

// ensures library can be used as intended outside the package scope
class ConsumerTest extends FlatSpec with Matchers {

  "Fusion syntax" should "work outside the package scope" in {
    val input = LazyList(1,2,3)

    input.map(_ + 1) shouldBe input.fuse.map(_ + 1).toLazyList
    input.map(_ + 1).toList shouldBe input.fuse.map(_ + 1).toList
    input.map(_ + 1).toVector shouldBe input.fuse.map(_ + 1).toVector
  }


}
