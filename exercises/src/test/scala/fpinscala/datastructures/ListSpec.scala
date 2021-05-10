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

    "3.12" in {
      List.reverse(List(1,2,3,4)) shouldBe List(4,3,2,1)
    }

    "3.13" in {
      List.foldRight2(List(1,2,3,4), 10)(_ + _) shouldBe 20
    }

    "3.14" in {
      List.append2(List(1,1,1), List(2,2,2)) shouldBe List(1,1,1,2,2,2)
    }

    "3.15" in {
      List.flatten(List(List(1,2,3), List(4,5,6), List(7,8,9))) shouldBe List(1,2,3,4,5,6,7,8,9)
    }

    "3.16" in {
      List.add1(List(1,2,3)) shouldBe List(2,3,4)
    }

    "3.17" in {
      List.doubleToString(List(1.0,2.0,3.0)) shouldBe List("1.0","2.0","3.0")
    }

    "3.18" in {
      List.map(List(1,2,3))(_*2) shouldBe List(2,4,6)
    }

    "3.19" in {
      List.filter(List(1,3,6,7,2,3))(_%2==0) shouldBe List(6,2)
    }

    "3.20" in {
      List.flatMap(List(1,2,3))(i => List(i, i)) shouldBe List(1,1,2,2,3,3)
    }

    "3.21" in {
      List.filter2(List(1,3,6,7,2,3))(_%2==0) shouldBe List(6,2)
    }

    "3.22" in {
      List.add(List(1,2,3),List(4,5,6)) shouldBe List(5,7,9)
    }

    "3.23" in {
      List.zipWith(List(1,2,3),List(4,5,6))(_+_) shouldBe List(5,7,9)
    }

    "3.24" in {
      List.hasSubsequence(List(1,2,3,4), List(1,2)) shouldBe true
      List.hasSubsequence(List(1,2,3,4), List(3,4)) shouldBe true
      List.hasSubsequence(List(1,2,3,4), List(1,1)) shouldBe false
      List.hasSubsequence(List(1,2,3,4), List(2,3,5)) shouldBe false
    }
  }
}
