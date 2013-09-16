package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tl: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * exercise2
   */
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("cannot apply tail")
      case Cons(_,t) => t
    }

  /**
   * exercise3
   */
  def setHead[A](l: List[A])(h: A): List[A] =
    l match {
      case Nil => sys.error("cannot apply setHead")
      case Cons(_,t) => Cons(h, t)
    }

  /**
   * exercise4
   */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n - 1)
    }

  /**
   * exercise5
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
  }
}
