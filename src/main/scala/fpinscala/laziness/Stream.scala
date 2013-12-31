package fpinscala.laziness

import Stream._

sealed abstract class Stream[+A] {
  def uncons: Option[Cons[A]]
  def isEmpty: Boolean = uncons.isEmpty

  /**
   * exercise1
   */
  def toList: List[A]

  /**
   * exercise2
   */
  def take(n: Int): Stream[A]

  /**
   * exercise3
   */
  def takeWhile(p: A => Boolean): Stream[A]
}

object Empty extends Stream[Nothing] {
  val uncons = None
  val toList = Nil
  def take(n: Int): Stream[Nothing] = this
  def takeWhile(p: Nothing => Boolean): Stream[Nothing] = this
}

sealed abstract class Cons[+A] extends Stream[A] {
  def head: A
  def tail: Stream[A]
  val uncons = Some(this)
  def toList: List[A] = head :: tail.toList
  def take(n: Int): Stream[A] =
    if (n == 0) Empty else Stream.cons(head, tail.take(n - 1))

  def takeWhile(p: A => Boolean): Stream[A] =
    if (p(head)) Stream.cons(head, tail.takeWhile(p)) else Empty
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = hd
    lazy val tail = tl
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

}
