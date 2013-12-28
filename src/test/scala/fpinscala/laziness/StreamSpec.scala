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
}
