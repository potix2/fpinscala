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
  "sequence" should {
    "be None when a None exists in the passed list" in {
      Option.sequence(List(Some(1), None)) must_== None
    }
    "return Some list when no None element exists in the passed list" in {
      Option.sequence(List(Some(1), Some(2))) must_== Some(List(1,2))
    }
  }
  "traverse" should {
    "return Some list when none elements doesn't exist" in {
      Option.traverse(List(1,2,3))(x => Some(x)) must_== Some(List(1,2,3))
    }
    "be None when a none element exists" in {
      Option.traverse(List(1,2,3))(x => if ( x % 2 == 0) None else Some(x)) must_== None
    }
  }
  "sequence_1" should {
    "be None when a None exists in the passed list" in {
      Option.sequence_1(List(Some(1), None)) must_== None
    }
    "return Some list when no None element exists in the passed list" in {
      Option.sequence_1(List(Some(1), Some(2))) must_== Some(List(1,2))
    }
  }
}
