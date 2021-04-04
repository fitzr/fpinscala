package fpinscala.gettingstarted

import org.scalatest.{FreeSpec, Matchers}

class GettingstartedSpec extends FreeSpec with Matchers {

  "for MyModule" - {
    "exec main" in {
      MyModule.main(Array.empty)
    }

    "fib" in {
      MyModule.fib(0) shouldBe 0
      MyModule.fib(1) shouldBe 1
      MyModule.fib(2) shouldBe 1
      MyModule.fib(3) shouldBe 2
      MyModule.fib(4) shouldBe 3
      MyModule.fib(5) shouldBe 5
      MyModule.fib(6) shouldBe 8
      MyModule.fib(7) shouldBe 13
      MyModule.fib(8) shouldBe 21
      MyModule.fib(9) shouldBe 34
    }
  }

  "for PolymorphicFunctions" - {
    "isSorted" in {
      PolymorphicFunctions.isSorted[Int](Array(), (a, b) => a < b) shouldBe true
      PolymorphicFunctions.isSorted[Int](Array(1), (a, b) => a < b) shouldBe true
      PolymorphicFunctions.isSorted[Int](Array(1, 2), (a, b) => a < b) shouldBe true
      PolymorphicFunctions.isSorted[Int](Array(1, 2, 3), (a, b) => a < b) shouldBe true
      PolymorphicFunctions.isSorted[Int](Array(1, 3, 3), (a, b) => a <= b) shouldBe true
      PolymorphicFunctions.isSorted[Int](Array(1, 3, 3), (a, b) => a < b) shouldBe false
      PolymorphicFunctions.isSorted[Int](Array(1, 3, 2), (a, b) => a < b) shouldBe false
      PolymorphicFunctions.isSorted[String](Array("a", "b", "c"), (a, b) => a < b) shouldBe true
      PolymorphicFunctions.isSorted[String](Array("a", "b", "b"), (a, b) => a <= b) shouldBe true
      PolymorphicFunctions.isSorted[String](Array("a", "c", "c"), (a, b) => a < b) shouldBe false
      PolymorphicFunctions.isSorted[String](Array("a", "c", "b"), (a, b) => a < b) shouldBe false
    }
  }
}