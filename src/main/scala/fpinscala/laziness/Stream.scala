package fpinscala.laziness

import Stream._
import scala.annotation.tailrec

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
  def takeWhile_2(p: A => Boolean): Stream[A] =
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
}
