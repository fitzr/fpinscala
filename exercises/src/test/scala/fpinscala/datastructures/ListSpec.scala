package fpinscala.datastructures

import org.scalatest.{FreeSpec, Matchers}

class ListSpec extends FreeSpec with Matchers {

  "3.2" - {
    "3.1" in {
      val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }
      x shouldBe 3
    }
  }

  "3.3" - {
    "3.2" in {
      List.tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
      an[RuntimeException] should be thrownBy List.tail(Nil)
    }

    "3.3" in {
      List.setHead(List(1, 2, 3, 4), 0) shouldBe List(0, 2, 3, 4)
      an[RuntimeException] should be thrownBy List.setHead(Nil, 0)
    }

    "3.4" in {
      List.drop(List(1, 2, 3, 4, 5), 0) shouldBe List(1, 2, 3, 4, 5)
      List.drop(List(1, 2, 3, 4, 5), 3) shouldBe List(4, 5)
    }

    "3.5" in {
      List.dropWhile(List(2, 2, 3, 4, 5))(_ == 2) shouldBe List(3, 4, 5)
      List.dropWhile(List(1, 2, 3, 4, 5))(_ == 0) shouldBe List(1, 2, 3, 4, 5)
    }

    "3.6" in {
      List.init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
      List.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
    }

    "3.9" in {
      List.length(List(1, 2, 3)) shouldBe 3
      List.length(List(1, 2, 3, 4, 5)) shouldBe 5
      List.length(Nil) shouldBe 0
    }

    "3.10" in {
      List.foldLeft( List(1,2,3,4,5), 0)(_ + _) shouldBe 15
    }

    "3.11" in {
      List.sum3(List(1,2,3,4,5)) shouldBe 15
      List.product3(List(1,2,3,4,5)) shouldBe 120
      List.length2(List(1,2,3,4,5)) shouldBe 5
    }
  }
}
