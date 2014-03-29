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
    "return empty stream when take(0)" in {
      Stream(1,2,3,4).take(0).toList must_== List()
    }
  }
  "drop" should {
    "remove the first 2 elements" in {
      Stream(1,2,3,4).drop(2).toList must_== List(3,4)
    }
    "not remove any elements when drop(0)" in {
      Stream(1,2,3,4).drop(0).toList must_== List(1,2,3,4)
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
  "takeWhile_1" should {
    "return the all starting elements of a Stream that match the given predicate" in {
      Stream(1,2,3,4,3,2,1).takeWhile_1(_ < 3).toList must_== List(1,2)
    }
  }
  "headOption_1" should {
    "return Some the head element when the stream is not empty" in {
      Stream(1,2,3,4).headOption_1 must_== Some(1)
    }
    "return None when the stream is empty" in {
      Stream.empty.headOption_1 must_== None
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
    "return an infinite Stream of Fibonacci numbers:" in {
      Stream.fibs.take(7).toList must_== List(0,1,1,2,3,5,8)
    }
  }
  "unfold" should {
    "generate an infinite Stream of constant" in {
      Stream.unfold(5)((s) => Some((s,s))).take(5).toList must_== List(5,5,5,5,5)
    }
    "generate an infinite Stream of from" in {
      Stream.unfold(10)((s) => Some((s, s + 1))).take(5).toList must_== List(10,11,12,13,14)
    }
  }
  "fibs_1" should {
    "return an infinite Stream of Fibonacci numbers:" in {
      Stream.fibs_1.take(7).toList must_== List(0,1,1,2,3,5,8)
    }
  }
  "from_1" should {
    "return an infinite Stream of integers, starting from n, then n + 1, n + 2 ..." in  {
      Stream.from_1(10).take(5).toList must_== List(10,11,12,13,14)
    }
  }
  "constant_1" should {
    "return an infinite Stream of given value" in {
      Stream.constant_1(1).take(5).toList must_== List(1,1,1,1,1)
    }
  }
  "ones_1" should {
    "return an infinite Stream of 1" in {
      Stream.ones_1.take(5).toList must_== List(1,1,1,1,1)
    }
  }
  "map_1" should {
    "return a Stream that the all elements applied a function" in {
      Stream(1,2,3,4,5,6).map_1(_ * 2).toList must_== List(2,4,6,8,10,12)
    }
  }
  "take_1" should {
    "return the first 2 elements" in {
      Stream(1,2,3,4).take_1(2).toList must_== List(1,2)
    }
  }
  "takeWhile_2" should {
    "return the all starting elements of a Stream that match the given predicate" in {
      Stream(1,2,3,4,3,2,1).takeWhile_2(_ < 3).toList must_== List(1,2)
    }
  }
  "zip" should {
    "return a Stream that every two elements from input Streams at the same position" in {
      Stream.constant(1).zip(Stream.constant(2)).take(3).toList must_== List((1,2), (1,2), (1,2))
    }
    "return the finite Stream that is the same length as the input Stream" in {
      Stream(1,2,3).zip(Stream.constant(2)).toList must_== List((1,2), (2,2), (3,2))
    }
    "return the finite Stream that is the same length as the input Stream" in {
      Stream.constant(1).zip(Stream(1,2,3)).toList must_== List((1,1), (1,2), (1,3))
    }
  }
  "zipAll" should {
    "return the infinite Stream" in {
      Stream(1,2,3).zipAll(Stream.constant(2)).take(4).toList must_== List((Some(1),Some(2)), (Some(2),Some(2)), (Some(3),Some(2)), (None, Some(2)))
    }
  }
  "startsWith" should {
    "be true when one Stream is a prefix of another" in {
      Stream.startsWith(Stream(1,2,3), Stream(1,2)) must beTrue
    }
  }
  "tails" should {
    "returns the Stream of suffixes of the input sequence, starting with the original Stream" in {
      Stream(1,2,3).tails.map(_.toList).toList must_== List(List(1,2,3), List(2,3), List(3), List())
    }
  }
  "scanRight" should {
    "returns a Stream of the intermediate results" in {
      Stream(1,2,3).scanRight(0)(_ + _).toList must_== List(6,5,3,0)
    }
  }
}
