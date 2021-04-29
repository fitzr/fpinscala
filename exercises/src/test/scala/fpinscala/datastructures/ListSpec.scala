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
      List.tail(List(1, 2, 3, 4)) shouldEqual List(2, 3, 4)
      an[RuntimeException] should be thrownBy List.tail(Nil)
    }

    "3.3" in {
      List.setHead(List(1, 2, 3, 4), 0) shouldEqual List(0, 2, 3, 4)
      an[RuntimeException] should be thrownBy List.setHead(Nil, 0)
    }

    "3.4" in {
      List.drop(List(1, 2, 3, 4, 5), 0) shouldEqual List(1, 2, 3, 4, 5)
      List.drop(List(1, 2, 3, 4, 5), 3) shouldEqual List(4, 5)
    }

    "3.5" in {
      List.dropWhile[Int](List(2, 2, 3, 4, 5), 2 == _) shouldEqual List(3, 4, 5)
      List.dropWhile[Int](List(1, 2, 3, 4, 5), 0 == _) shouldEqual List(1, 2, 3, 4, 5)
    }

    "3.6" in {
      List.init(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3)
      List.init(List(1, 2, 3, 4, 5)) shouldEqual List(1, 2, 3, 4)
    }
  }
}
