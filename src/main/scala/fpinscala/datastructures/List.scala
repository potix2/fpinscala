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

  /**
   * exercise6
   */
  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
   * exercise10
   */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_,acc) => 1 + acc)

  /**
   * exercise11
   */
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  /**
   * exercise12
   */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h, acc))

  /**
   * exercise13
   */
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a,b) => f(b,a))

  /**
   * exercise14
   */
  def append2[A](as: List[A], bs: List[A]): List[A] = foldLeft(List.reverse(as), bs) { (b, a) => Cons(a,b) }

  /**
   * exercise15
   */
  def flatten[A](l: List[List[A]]): List[A] = foldLeft(l, List[A]())((b,a) => append(b,a))

  /**
   * exercise16
   */
  def addOne(l: List[Int]): List[Int] = foldRight(l, List[Int]())((a,b) => Cons(a + 1, b))

  /**
   * exercise17
   */
  def doubleToString(l: List[Double]): List[String] = foldRight(l, List[String]())((a,b) => Cons(a.toString(), b))

  /**
   * exercise18
   */
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((a,b) => Cons(f(a), b))

  /**
   * exercise19
   */
  def filter[A](l: List[A])(p: A => Boolean) : List[A] = foldRight(l, List[A]())((a,b) => if (p(a)) Cons(a, b) else b)

  /**
   * exercise20
   */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))

  /**
   * exercise21
   */
  def filter2[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(a => if (p(a)) List(a) else List())

  /**
   * exercise22
   */
  def zipWithAddition(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
    case (Cons(lh, lt), Cons(rh, rt)) => Cons(lh + rh, zipWithAddition(lt, rt))
    case _ => Nil
  }

  /**
   * exercise23
   */
  def zipWith[A,B](l: List[A], r: List[A])(f: (A,A) => B): List[B] = {
    def rec(ll: List[A], rr: List[A]): List[B] = (ll,rr) match {
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh), rec(lt, rt))
      case _ => Nil
    }

    rec(l,r)
  }

  /**
   * exercise24
   */
  def subsequence[A](l: List[A]): List[List[A]] = l match {
    case Cons(h, t) => Cons(l, subsequence(t))
    case _ => Nil
  }

  def exists[A](l: List[A])(p: A => Boolean): Boolean = foldLeft(l, false)((b,a) => b || p(a))

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def p(ll: List[A], ss: List[A]): Boolean = (ll, ss) match {
      case (Cons(h1,t1), Nil) => true
      case (Cons(h1,t1), Cons(h2,t2)) if h1 == h2 => p(t1,t2)
      case _ => false
    }

    //Streamを使うと効率的?
    exists(subsequence(l))(p(_,sub))
  }
}
