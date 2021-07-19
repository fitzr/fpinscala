package fpinscala.laziness

import org.scalatest.{FreeSpec, Matchers}
import fpinscala.laziness.Stream._

class StreamSpec extends FreeSpec with Matchers {

  "5.1" - {
    "use foldRight" in {
      cons(1, cons(2, Stream.empty)).toList shouldBe List(1, 2)
      Stream().toList shouldBe List.empty
    }

    "use recursive" in {
      cons(1, cons(2, Stream.empty)).toList2 shouldBe List(1, 2)
      Stream().toList2 shouldBe List.empty
    }
  }

  "5.2" - {
    "take" - {
      "take less than length" in {
        val taken = Stream(1, 2, 3).take(2)
        taken.toList shouldBe List(1, 2)
      }

      "take all" in {
        val taken = Stream(1, 2, 3).take(3)
        taken.toList shouldBe List(1, 2, 3)
      }

      "take over" in {
        val taken = Stream(1, 2, 3).take(4)
        taken.toList shouldBe List(1, 2, 3)

        // def func(x: => A)
        // func(h()) // h() is not executed. laziness.
        // func({print("test")}) // print is not executed. laziness.
      }
    }

    "drop" - {
      "drop less than length" in {
        val dropped = Stream(1, 2, 3).drop(1)
        dropped.toList shouldBe List(2, 3)
      }

      "drop all" in {
        val dropped = Stream(1, 2, 3).drop(3)
        dropped.toList shouldBe List()
      }

      "drop over" in {
        val dropped = Stream(1, 2, 3).drop(4)
        dropped.toList shouldBe List()
      }
    }
  }

  "5.3" in {
    val taken = Stream(1, 2, 3, 4).takeWhile(_ < 3)
    taken.toList shouldBe List(1, 2)
  }

  "5.4" - {
    "false" in {
      val s = cons(2, cons(1, cons({ print("laziness"); 2 }, Empty)))
      s.forAll(_ == 2) shouldBe false
    }

    "true" in {
      val s = cons(2, cons(2, cons({ print("laziness"); 2 }, Empty)))
      s.forAll(_ == 2) shouldBe true
    }
  }

  "5.5" in {
    val taken = Stream(1, 2, 3, 4).takeWhile2(_ < 3)
    taken.toList shouldBe List(1, 2)
  }

  "5.6" in {
    Stream(1, 2, 3, 4).headOption shouldBe Some(1)
    Empty.headOption shouldBe None
  }

  "5.7" - {
    "map" in {
      Stream(1, 2, 3, 4).map(_ * 2).toList shouldBe List(2, 4, 6, 8)
    }

    "filter" in {
      Stream(3, 2, 1, 4).filter(_ < 3).toList shouldBe List(2, 1)
    }

    "append" in {
      Stream(1, 2, 3, 4).append(Stream(5)).toList shouldBe List(1, 2, 3, 4, 5)
    }

    "flatMap" in {
      Stream(1, 2, 3, 4).flatMap(a => Stream(a * 2)).toList shouldBe List(2, 4, 6, 8)
    }
  }

  "5.8" in {
    Stream.constant(5).take(3).toList shouldBe List(5, 5, 5)
  }

  "5.9" in {
    Stream.from(5).take(3).toList shouldBe List(5, 6, 7)
  }

  "5.10" in {
    Stream.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "5.11" in {
    Stream.unfold[String, Int](2)(a => Some((a.toString, a * 3))).take(3).toList shouldBe List("2", "6", "18")
  }

  "5.12" - {
    "fibs2" in {
      Stream.fibs2.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    }

    "from2" in {
      Stream.from2(5).take(3).toList shouldBe List(5, 6, 7)
    }

    "constant2" in {
      Stream.constant2(5).take(3).toList shouldBe List(5, 5, 5)
    }

    "ones2" in {
      Stream.ones2.take(3).toList shouldBe List(1, 1, 1)
    }
  }

  "5.13" - {
    "map2" in {
      Stream(1, 2, 3, 4).map2(_ * 2).toList shouldBe List(2, 4, 6, 8)
    }

    "take2" in {
      Stream(1, 2, 3, 4).take2(2).toList shouldBe List(1, 2)
      Stream(1, 2, 3, 4).take2(5).toList shouldBe List(1, 2, 3, 4)
    }

    "takeWhile3" in {
      Stream(1, 2, 3, 4).takeWhile3(_ < 3).toList shouldBe List(1, 2)
    }

    "zipWith" in {
      Stream(1, 2, 3, 4).zipWith(Stream(2, 3, 4, 5))(_ + _).toList shouldBe List(3, 5, 7, 9)
    }

    "zipApp" in {
      Stream(1, 2).zipAll(Stream(3, 4, 5, 6)).toList shouldBe
        List((Some(1), Some(3)), (Some(2), Some(4)), (None, Some(5)), (None, Some(6)))
    }
  }

  "5.14" in {
    Stream(1,2,3).startsWith(Stream(1, 2)) shouldBe true
    Stream(1,3,3).startsWith(Stream(1, 2)) shouldBe false
    Stream(1,3,3).startsWith(Stream(1, 3, 3, 3)) shouldBe false
  }

  "5.15" in {
    Stream(1,2,3).tails.map(_.toList).toList shouldBe List(List(1,2,3), List(2, 3), List(3), List())
  }

  "5.16" in {
    Stream(1,2,3).scanRight(0)(_ + _).toList shouldBe List(6,5,3,0)
  }
}