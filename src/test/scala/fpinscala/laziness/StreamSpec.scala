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
}
