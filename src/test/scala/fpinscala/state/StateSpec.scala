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
}
