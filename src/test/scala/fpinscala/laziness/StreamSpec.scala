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
  "filter" should {
    "return new Stream that the all elements matching a Boolean function" in {
      Stream(1,2,3,4,5,6).filter(_ % 2 == 0).toList must_== List(2,4,6)
    }
  }
  "map" should {
    "return a Stream that the all elements applied a function" in {
      Stream(1,2,3,4,5,6).map(_ * 2).toList must_== List(2,4,6,8,10,12)
    }
  }
  "append" should {
    "concatenate two Streams" in {
      Stream(1,2,3).append(Stream(4,5,6)).toList must_== List(1,2,3,4,5,6)
    }
  }
  "flatten" should {
    "make a flat Stream from the nested Stream" in {
      Stream(Stream(1), Stream(2), Stream(3)).flatten.toList must_== List(1,2,3)
    }
  }
  "flatMap" should {
    "return a flatten stream that the all elements applied a function" in {
      Stream(1,2,3).flatMap(Stream(_)).toList must_== List(1,2,3)
    }
  }
  "constant" should {
    "return an infinite Stream of given value" in {
      Stream.constant(1).take(5).toList must_== List(1,1,1,1,1)
    }
  }
  "from" should {
    "return an infinite Stream of integers, starting from n, then n + 1, n + 2 ..." in  {
      Stream.from(10).take(5).toList must_== List(10,11,12,13,14)
    }
  }
  "fibs" should {
    "return an inifinite Stream of Fibonacci numbers:" in {
      Stream.fibs.take(7).toList must_== List(0,1,1,2,3,5,8)
    }
  }
}
