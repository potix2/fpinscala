package fpinscala.state

import org.specs2.mutable.Specification
import fpinscala.state.RNG.Simple

class StateSpec extends Specification {
  "positiveInt" should {
    "return a positive integer" in {
      RNG.positiveInt(Simple(42))._1 must_== 16159453
    }
    "return minimum integer when previous value is negative" in {
      RNG.positiveInt(Simple(-1))._1 must_== 384748
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
      d must beCloseTo(0.596735 +/- 0.00001)
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
      d1 must beCloseTo(0.007524 +/- 0.00001)
      d2 must beCloseTo(0.596735 +/- 0.00001)
      d3 must beCloseTo(0.158467 +/- 0.00001)
    }
  }

  "ints" should {
    "return a list of random integers" in {
      RNG.ints(3)(Simple(42))._1 must_== List(-340305902,-1281479697,16159453)
    }
  }

  "double_1" should {
    "return a double between 0 and 1" in {
      RNG.double_1(RNG.Simple(43))._1 must beCloseTo(0.007703 +/- 0.00001)
    }
  }

  "map2" should {
    "apply a function to two random integers" in {
      (RNG.map2(RNG.unit(3), RNG.unit(2))((a,b) => a * b))(Simple(2))._1 must_== 6
    }
  }

  "sequence" should {
    "combine a list of Rand into one Rand containing a list of all the random values" in {
      RNG.sequence(List(RNG.unit(1), RNG.unit(2)))(Simple(0))._1 must_== List(1,2)
    }
  }

  "positiveLessThan" should {
    "generate an integer between 0 and n" in {
      RNG.positiveLessThan(2)(RNG.Simple(1))._1 must_== 0
    }
  }

  "map_1" should {
    "return a double between 0 and 1" in {
      (RNG.map_1(RNG.positiveInt)(i => i / (Int.MaxValue.toDouble + 1))(RNG.Simple(43)))._1 must beCloseTo(0.007703 +/- 0.00001)
    }
  }

  "map2_1" should {
    "apply a function to two random integers" in {
      (RNG.map2_1(RNG.unit(3), RNG.unit(2))((a,b) => a * b))(Simple(2))._1 must_== 6
    }
  }

}
