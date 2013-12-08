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

  "maximum" should {
    "return a value with a leaf" in {
      Tree.maximum(Leaf(1)) must_== 1
    }
    "return the max value in a tree" >> {
      Tree.maximum(Branch(Leaf(1), Leaf(2))) must_== 2
      Tree.maximum(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) must_== 3
    }
  }
}
