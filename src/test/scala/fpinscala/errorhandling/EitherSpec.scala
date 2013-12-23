package fpinscala.errorhandling

import org.specs2.mutable.Specification

class EitherSpec extends Specification {
  "map" should {
    "apply the function when the value is Right" in {
      Right(1).map(_ + 1) must_== Right(2)
    }
    "not apply the function when the value is Left" in {
      val l: Either[Int, Int] = Left(1)
      l.map(_ + 1) must_== Left(1)
    }
  }
  "flatMap" should {
    "apply the function when the value is Right" in {
      Right(1).flatMap(a => Right(a + 1)) must_== Right(2)
    }
    "not apply the function when the value is Left" in {
      val l: Either[Int, Int] = Left(1)
      l.flatMap(a => Right(a)) must_== Left(1)
    }
  }
  "orElse" should {
    "return Right value when the value is Right" in {
      Right(1).orElse(Right(0)) must_== Right(1)
    }
    "return default value when the value is Left" in {
      val l: Either[Int, Int] = Left(1)
      l.orElse(Right(0)) must_== Right(0)
    }
  }
  "map2" should {
    "apply the function when subject and passed value are Right" in {
      Right(1).map2(Right(2))(_ + _) must_== Right(3)
    }
    "not apply the function when passed value is Left" in {
      Right(1).map2(Left(2))(_ + _) must_== Left(2)
    }
    "not apply the function when subject value is Left" in {
      val l: Either[Int, Int] = Left(1)
      l.map2(Right(2))(_ + _) must_== Left(1)
    }
  }
}
