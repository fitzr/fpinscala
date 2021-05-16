package fpinscala.datastructures

import org.scalatest.{FreeSpec, Matchers}

class TreeSpec extends FreeSpec with Matchers {
  "3-4" in {
    Tree.size(Branch(Branch(Leaf(1), Leaf(1)),Leaf(1))) shouldBe 5
  }

  "3-5" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(3)),Leaf(2))) shouldBe 3
  }

  "3-6" in {
    Tree.depth(Branch(Branch(Leaf(1), Leaf(3)),Leaf(2))) shouldBe 2
  }

  "3-7" in {
    Tree.map(Branch(Branch(Leaf(1), Leaf(3)),Leaf(2)))(_.toString)
      .shouldBe(Branch(Branch(Leaf("1"), Leaf("3")),Leaf("2")))
  }

  "3-8" - {
    "3-4" in {
      Tree.size2(Branch(Branch(Leaf(1), Leaf(1)),Leaf(1))) shouldBe 5
    }

    "3-5" in {
      Tree.maximum2(Branch(Branch(Leaf(1), Leaf(3)),Leaf(2))) shouldBe 3
    }

    "3-6" in {
      Tree.depth2(Branch(Branch(Leaf(1), Leaf(3)),Leaf(2))) shouldBe 2
    }

    "3-7" in {
      Tree.map2(Branch(Branch(Leaf(1), Leaf(3)),Leaf(2)))(_.toString)
        .shouldBe(Branch(Branch(Leaf("1"), Leaf("3")),Leaf("2")))
    }
  }
}
