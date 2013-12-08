package fpinscala.datastructures

import org.specs2.mutable.Specification

class TreeSpec extends Specification {
  "size" should {
    "be one with a leaf" in {
      Tree.size(Leaf(1)) must_== 1
    }
    "be three with a branch and two leaves" in {
      Tree.size(Branch(Leaf(1),Leaf(2))) must_== 3
    }
  }
}
