package fpinscala.laziness

import org.specs2.mutable.Specification

class StreamSpec extends Specification {
  "toList" should {
    "convert a Empty to Nil" in {
      Stream().toList must_== List()
    }
    "convert a Stream to a List" in {
      Stream(1,2,3,4).toList must_== List(1,2,3,4)
    }
  }
  "take" should {
    "return the first 2 elements" in {
      Stream(1,2,3,4).take(2).toList must_== List(1,2)
    }
  }
  "takeWhile" should {
    "return the all starting elements of a Stream that match the given predicate" in {
      Stream(1,2,3,4,3,2,1).takeWhile(_ < 3).toList must_== List(1,2)
    }
  }
  "exists" should {
    "be true when an element matching a Boolean function exists" in {
      Stream(1,2,3,4,5).exists(_ % 2 == 0) must beTrue
    }
    "be false when an element matching a Boolean function exists" in {
      Stream(1,2,3,4,5).exists(_ > 5) must beFalse
    }
  }
  "forAll" should {
    "be true when the all elements match a given predicate" in {
      Stream(2,4,6,8,10).forAll(_ % 2 == 0) must beTrue
    }
    "be false when an element not matching a Boolean function exists" in {
      Stream(2,4,6,8,11,12).forAll(_ % 2 == 0) must beFalse
    }
  }
  "takeWhile_2" should {
    "return the all starting elements of a Stream that match the given predicate" in {
      Stream(1,2,3,4,3,2,1).takeWhile_2(_ < 3).toList must_== List(1,2)
    }
  }
}
