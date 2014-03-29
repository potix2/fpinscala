package fpinscala.laziness

import scala.{Stream => _}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
   * exercise1
   */
  def toList: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }

  /**
   * exercise2
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case s if n == 0 => s
    case _ => Stream.empty
  }

  /**
   * exercise3
   */
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /**
   * exercise4
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  /**
   * exercise5
   */
  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  /**
   * exercise6
   */
  def headOption_1: Option[A] =
    foldRight(None: Option[A])((a,b) => Some(a))

  /*
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  */
  /**
   * exercise7
   */
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b) => if (p(a)) Stream.cons(a, b) else b)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))

  def append[B>:A](c: Stream[B]): Stream[B] =
    foldRight(c)((a,b) => Stream.cons(a,b))

  /**
   * @see http://scabl.blogspot.jp/2013/02/monads-in-scala-1.html
   */
  def flatten[B](implicit asStreamStream: Stream[A] <:< Stream[Stream[B]]): Stream[B] =
    asStreamStream(this).foldRight(Stream.empty[B])((a,b) => a.append(b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    map(f).flatten

  /**
   * exercise13
   */
  def map_1[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case Empty => None
    }

  def take_1(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h,t), x) if x > 0 => Some((h(), (t(), x - 1)))
      case _ => None
    }

  def takeWhile_2(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zip[B](c: Stream[B]): Stream[(A,B)] =
    zipWith(c)((_,_))

  def zipWith[B, C](c: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, c)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s)((_,_))

  def zipWithAll[B,C](s: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    val a = this.map (Some(_)) append Stream.constant(None)
    val b = s.map (Some(_)) append Stream.constant(None)
    Stream.unfold((a,b)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(), t2())))
      case _ => None
    }
  }

  /**
   * exercise 15
   */
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Cons(h,t) => Some((Stream.cons(h(), t()), t()))
      case _ => None
    } append Stream.cons(Stream.empty[A], Stream.empty)

  /**
   * exercise 16
   * unfoldを使って実装することはできない。O(n)になるように実装するためにはaccumulatorを使う方法が考えられる。ここで、結合性に着目すると
   * scanRightは右結合的でunfoldは左結合的なのでscanRightをunfoldとaccumulatorを使って実装することはできない。
   *
   * ex. Stream(1,2,3).scanRight(0)(_ + _)
   *
   *    f(1, f(2, f(3, (0, 0))))
   *              ^^^^^^^^^^^^
   * => f(1, f(2, (3, 3 :: 0)))
   *         ^^^^^^^^^^^^^^^^^
   * => f(1, (5, 5 :: 3 :: 0))
   *    ^^^^^^^^^^^^^^^^^^^^^^
   * => (6, 6 :: 5 :: 3 :: 0)
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a,b) => {
      val b2 = f(a, b._1)
      (b2, Stream.cons(b2, b._2))
    })._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
   * exercise 8
   */
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  /**
   * exercise 9
   */
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  val ones: Stream[Int] = constant(1)

  /**
   * exercise 10
   */
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = Stream.cons(a, go(b, a + b))
    go(0,1)
  }

  /**
   * exercise 11
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a,unfold(s)(f))
      case None => empty
    }

  /**
   * exercise 12
   */
  def fibs_1: Stream[Int] = unfold((0,1))((s) => Some(s._1, (s._2, s._1 + s._2)))
  def from_1(n: Int): Stream[Int] = unfold(n)((s) => Some((s, s + 1)))
  def constant_1[A](a: A): Stream[A] = unfold(a)((s) => Some((s,s)))
  val ones_1 = unfold(1)((s) => Some((s,1)))

  /**
   * exercise 14
   */
  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    s.zip(s2).forAll(a => a._1 == a._2)
}
