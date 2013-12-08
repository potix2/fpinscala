package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * exercise25
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  /**
   * exercise26
   */
  def maximum(t: Tree[Int]): Int = {
    def rec(tt: Tree[Int], m: Int): Int = tt match {
      case Leaf(v) => m.max(v)
      case Branch(l,r) => rec(r, rec(l, m))
    }
    rec(t, Int.MinValue)
  }
}
