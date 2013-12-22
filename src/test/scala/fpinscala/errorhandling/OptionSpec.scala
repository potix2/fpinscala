package fpinscala.errorhandling

import org.specs2.mutable.Specification

class OptionSpec extends Specification {
  "variance" should {
    "be None with empty sequence" in {
      Option.variance(List()) must_== None
    }
    "return a variance with sequence" in {
      Option.variance(List(3,3,4,2,2,1,3,2,3,4,4,5)).getOrElse(0.0) must beCloseTo(1.1666666 +/- 0.00001)
    }
  }
  "map2" should {
    "be None when None is passed as argument" in {
      Option.map2(Some(1), None)(_ + _) must_== None
    }
    "apply the binary function when some values are passed" in {
      Option.map2(Some(1), Some(2))(_ + _) must_== Some(3)
    }
  }
  "bothMatch" should {
    "be None when a pattern is invalid" in {
      Option.bothMatch("[a-z]+", "][", "abcd") must_== None
    }
    "be Some(false) when the patterns are valid and a subject string is not matched" in {
      Option.bothMatch("[a-z]+", "[0-9]+", "abcd") must_== Some(false)
    }
    "be Some(true) when the patterns are valid and a subject string is matched" in {
      Option.bothMatch("[a-z]+", "[a-z]+", "abcd") must_== Some(true)
    }
  }
}
