package fpinscala.errorhandling

import org.scalatest.{FreeSpec, Matchers}

class OptionSpec extends FreeSpec with Matchers {
  "4-1" - {
    "map" in {
      Some(4).map(_ * 2) shouldBe Some(8)
      (None: Option[Int]).map[Int](_ * 2) shouldBe None
    }

    "getOrElse" in {
      Some(4).getOrElse(0) shouldBe 4
      None.getOrElse(0) shouldBe 0
    }

    "flatMap" in {
      Some(4).flatMap(v => Some(v * 2)) shouldBe Some(8)
      (None: Option[Int]).flatMap[Int](v => Some(v * 2)) shouldBe None
    }

    "orElse" in {
      Some(4).orElse(Some(0)) shouldBe Some(4)
      None.orElse(Some(0)) shouldBe Some(0)
    }

    "filter" in {
      Some(4).filter(_ > 0) shouldBe Some(4)
      Some(-1).filter(_ > 0) shouldBe None
      (None: Option[Int]).filter(_ > 0) shouldBe None
    }
  }

  "4-2" in {
    Option.variance(Seq.empty) shouldBe None
    Option.variance(Seq(1,1,1)) shouldBe Some(0)
  }

  "4-3" in {
    Option.map2(Some(1), Some(1))(_ + _) shouldBe Some(2)
    Option.map2[Int, Int, Int](None, Some(1))(_ + _) shouldBe None
    Option.map2(Some(1), None)(_ + _) shouldBe None
    Option.map2[Int, Int, Int](None, None)(_ + _) shouldBe None
  }

  "4-4" in {
    Option.sequence(List(Some(1), Some(2))) shouldBe Some(List(1,2))
    Option.sequence(List(Some(1), None)) shouldBe None
  }

  "4.5" in {
    Option.traverse(List(1, 2))(a => if (a > 0) Some(a) else None) shouldBe Some(List(1,2))
    Option.traverse(List(1, 0))(a => if (a > 0) Some(a) else None) shouldBe None
  }
}
