package fpinscala.laziness

import org.scalatest.{FreeSpec, Matchers}
import fpinscala.laziness.Stream._

class StreamSpec extends FreeSpec with Matchers {

  "5.1" - {
    "use foldRight" in {
      cons({1}, cons({2}, Stream.empty)).toList shouldBe List(1, 2)
      Stream().toList shouldBe List.empty
    }

    "use recursive" in {
      cons({1}, cons({2}, Stream.empty)).toList2 shouldBe List(1, 2)
      Stream().toList2 shouldBe List.empty
    }
  }

  "5.2" - {
    "take" - {
      "take less than length" in {
        val taken = Stream({1}, {2}, {3}).take(2)
        taken.toList shouldBe List(1, 2)
      }

      "take all" in {
        val taken = Stream({1}, {2}, {3}).take(3)
        taken.toList shouldBe List(1, 2, 3)
      }

      "take over" in {
        val taken = Stream({1}, {2}, {3}).take(4)
        taken.toList shouldBe List(1, 2, 3)

        // def func(x: => A)
        // func(h()) // h() is not executed. laziness.
      }
    }
  }

}
