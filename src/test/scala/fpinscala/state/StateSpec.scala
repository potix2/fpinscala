package fpinscala.state

import org.specs2.mutable.Specification

class StateSpec extends Specification {
  "positiveInt" should {
    "return a positive integer" in {
      RNG.positiveInt(Simple(42))._1 must_== 16159453
    }
    "return minimum integer when previous value is negative" in {
      RNG.positiveInt(Simple(-1))._1 must_== Int.MinValue
    }
  }

  "double" should {
    "return a double between 0 and 1" in {
      RNG.double(Simple(43))._1 must beCloseTo(0.007703 +/- 0.00001)
    }
  }

  "intDouble" should {
    "return a tuple of an integer and a double with RNG" in {
      val ((i, d), rng) = RNG.intDouble(Simple(42))
      i must_== 16159453
      d must beCloseTo(-0.596735 +/- 0.00001)
    }
  }

  "doubleInt" should {
    "return a tuple of an integer and a double with RNG" in {
      val ((d, i), rng) = RNG.doubleInt(Simple(42))
      i must_== -1281479697
      d must beCloseTo(0.0075248 +/- 0.00001)
    }
  }

  "double3" should {
    "return a triple of the double values with RNG" in {
      val ((d1, d2, d3), rng) = RNG.double3(Simple(42))
      d1 must beCloseTo( 0.007524 +/- 0.00001)
      d2 must beCloseTo(-0.596735 +/- 0.00001)
      d3 must beCloseTo(-0.158467 +/- 0.00001)
    }
  }

  "ints" should {
    "return a list of random integers" in {
      RNG.ints(3)(Simple(42))._1 must_== List(-340305902,-1281479697,16159453)
    }
  }
}
