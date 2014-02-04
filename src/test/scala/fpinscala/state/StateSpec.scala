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
}
