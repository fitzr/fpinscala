package fpinscala.errorhandling

import org.scalatest.{FreeSpec, Matchers}

class EitherSpec extends FreeSpec with Matchers {

  "4.6" - {
    "map" in {
      Right(2).map(_ * 2) shouldBe Right(4)
      (Left("err"): Either[String, Int]).map(_ * 2) shouldBe Left("err")
    }

    "flatMap" in {
      Right(2).flatMap(v => Right(v * 2)) shouldBe Right(4)
      Right(2).flatMap(v => Left("err")) shouldBe Left("err")
      (Left("err"): Either[String, Int]).flatMap(v => Right(v * 2)) shouldBe Left("err")
    }

    "orElse" in {
      Right(2).orElse(Right(0)) shouldBe Right(2)
      (Left("err"): Either[String, Int]).orElse(Right(0)) shouldBe Right(0)
      (Left("err"): Either[String, Int]).orElse(Left("err2")) shouldBe Left("err2")
    }

    "map2" in {
      Right(2).map2(Right(3))(_ * _) shouldBe Right(6)
      Right(2).map2(Left("err"))((a, b: Int) => a * b) shouldBe Left("err")
      (Left("err"): Either[String, Int]).map2(Right(2))(_ * _) shouldBe Left("err")
      (Left("err"): Either[String, Int]).map2(Left("err2"))((a, b: Int) => a * b) shouldBe Left("err")
    }
  }

  "4.7" - {
    "sequence" in {
      Either.sequence(List(Right(2), Right(3))) shouldBe Right(List(2, 3))
      Either.sequence(List(Right(2), Left("err"))) shouldBe Left("err")
      Either.sequence(List(Left("err1"), Left("err2"))) shouldBe Left("err1")
    }

    "traverse" in {
      Either.traverse(List(1, 2, 3))(v => Right(v * 2)) shouldBe Right(List(2, 4, 6))
      Either.traverse(List(1, 2, 3)) {
        case 2 => Left("err")
        case v => Right(v)
      } shouldBe Left("err")
    }
  }
}
