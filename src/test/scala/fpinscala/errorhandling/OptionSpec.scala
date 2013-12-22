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
}
