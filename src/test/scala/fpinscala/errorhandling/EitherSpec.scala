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
  "traverse" should {
    "return Right list when Left elements doesn't exist" in {
      Either.traverse(List(1,2,3))(x => Right(x)) must_== Right(List(1,2,3))
    }
    "be None when a none element exists" in {
      Either.traverse(List(1,2,3))(x => if ( x % 2 == 0) Left(x) else Right(x)) must_== Left(2)
    }
  }
  "sequence" should {
    "be Left value when a Left exists in the passed list" in {
      Either.sequence(List(Right(1), Left("fail"))) must_== Left("fail")
    }
    "return Right list when no Left value exists in the passed list" in {
      Either.sequence(List(Right(1), Right(2))) must_== Right(List(1,2))
    }
  }
}
