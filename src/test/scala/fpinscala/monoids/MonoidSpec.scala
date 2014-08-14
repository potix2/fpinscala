package fpinscala.monoids

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck._

class MonoidSpec extends Specification with ScalaCheck {
  "String satisfies monoid laws" ! Monoid.monoidLaws(Monoid.stringMonoid, Gen.identifier)
  "List satisfies monoid laws" ! Monoid.monoidLaws(Monoid.listMonoid[String], Gen.listOf(Gen.identifier))
}
