package fpinscala.laziness

import Stream._

sealed abstract class Stream[+A] {
  def uncons: Option[Cons[A]]
  def isEmpty: Boolean = uncons.isEmpty

  /**
   * exercise1
   */
  def toList: List[A] =
    uncons match {
      case Some(c) => c.head :: c.tail.toList
      case None => Nil
    }

  /**
   * exercise2
   */
  def take(n: Int): Stream[A] =
    if (n > 0) uncons match {
      case Some(c) if (n == 1) => cons(c.head, empty)
      case Some(c) => cons(c.head, c.tail.take(n - 1))
      case _ => empty
    }
    else empty

  /**
   * exercise3
   */
  def takeWhile(p: A => Boolean): Stream[A] =
    uncons match {
      case Some(c) if (p(c.head)) => cons(c.head, c.tail.takeWhile(p))
      case _ => empty
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some(c) => f(c.head, c.tail.foldRight(z)(f))
      case None => z
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
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty)

  /**
   * exercise6
   */
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else b)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def append[B>:A](c: Stream[B]): Stream[B] =
    foldRight(c)((a,b) => cons(a,b))

  /**
   * @see http://scabl.blogspot.jp/2013/02/monads-in-scala-1.html
   */
  def flatten[B](implicit asStreamStream: Stream[A] <:< Stream[Stream[B]]): Stream[B] =
    asStreamStream(this).foldRight(empty[B])((a,b) => a.append(b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    map(f).flatten

  /**
   * exercise13
   */
  def map_1[B](f: A => B): Stream[B] =
    unfold(this)((s) =>
      s.uncons match {
        case Some(c) => Some((f(c.head), c.tail))
        case None => None
      })

  def take_1(n: Int): Stream[A] =
    unfold((this, n))((s) =>
      if (s._2 > 0)
        s._1.uncons match {
          case Some(c) => Some((c.head, (c.tail, s._2 - 1)))
          case _ => None
        }
      else None)

  def takeWhile_2(p: A => Boolean): Stream[A] =
    unfold(this)((s) =>
      s.uncons match {
        case Some(c) if (p(c.head)) => Some((c.head, c.tail))
        case _ => None
      })

  def zip[B](c: Stream[B]): Stream[(A,B)] =
    zipWith(c)((_,_))

  def zipWith[B, C](c: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, c)) { case (s1,s2) =>
      (s1.uncons, s2.uncons) match {
        case (Some(a), Some(b)) => Some((f(a.head, b.head), (a.tail, b.tail)))
        case _ => None
      }
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s)((_,_))

  def zipWithAll[B,C](s: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    val a = this map (Some(_)) append constant(None)
    val b = s map (Some(_)) append constant(None)
    unfold((a,b)) {
      case (s1,s2) => for {
        c1 <- s1.uncons
        c2 <- s2.uncons
      } yield (f(c1.head, c2.head), (c1.tail, c2.tail))
    }
  }

  /**
   * exercise 15
   */
  def tails: Stream[Stream[A]] =
    unfold(this)(s => s.uncons match {
      case Some(c) => Some(s, c.tail)
      case _ => None
    }) append(Stream(empty))

  /**
   * exercise 16
   * unfoldを使って実装することはできない。O(n)になるように実装するためにはaccumulatorを使う方法が考えられる。ここで、結合性に着目すると
   * scanRightは右結合的でunfoldは左結合的なのでscanRightをunfoldとaccumulatorを使って実装することはできない。
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a,b) => {
      val b2 = f(a, b._1)
      (b2, cons(b2, b._2))
    })._2

  def scanRightViaUnfold[B](z: B)(f: (A, => B) => B): Stream[B] =
    unfold(this)(s => s.uncons match {
      case Some(c) => Some(s.foldRight(z)(f), c.tail)
      case _ => None
    }) append(Stream(z))
}

object Empty extends Stream[Nothing] {
  val uncons = None
}

sealed abstract class Cons[+A] extends Stream[A] {
  def head: A
  def tail: Stream[A]
  val uncons = Some(this)
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = hd
    lazy val tail = tl
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  /**
   * exercise 8
   */
  def constant[A](a: A): Stream[A] = new Cons[A] {
    val head = a
    lazy val tail = this
  }

  /**
   * exercise 9
   */
  def from(n: Int): Stream[Int] = new Cons[Int] {
    val head = n
    lazy val tail = from(n+1)
  }

  val ones: Stream[Int] = constant(1)

  /**
   * exercise 10
   */
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = new Cons[Int] {
      val head = a
      lazy val tail = go(b, a + b)
    }
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
