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

  "depth" should {
    "be one with a leaf" in {
      Tree.depth(Leaf(1)) must_== 1
    }
    "return the maximum path length from the root of a tree to any leaf" >> {
      Tree.depth(Branch(Leaf(1), Leaf(2))) must_== 2
      Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) must_== 3
      Tree.depth(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))) must_== 4
    }
  }

  "map" should {
    "returns elements applied a function" >> {
      Tree.map(Leaf(1))(_ * 2) must_== Leaf(2)
      Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ * 2) must_== Branch(Leaf(2), Branch(Leaf(4), Leaf(6)))
    }
  }

  "fold" should {
    "returns new elements" >> {
      Tree.fold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))((a) => List(a))((l,r) => List.append(l,r)) must_== List(1,2,3)
    }
  }
}
