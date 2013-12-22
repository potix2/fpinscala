package fpinscala.errorhandling

import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap( a => if(f(a)) Some(a) else None )

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {


  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
   * exercise2
   */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
   * exercise3
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap(x => b.map(f.curried(x)))

  /**
   * exercise4
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap(hh => sequence(t) map(hh :: _) ) //cf. map2!!!
  }

  /**
   * exercise5
   */
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequence_1[A](s: List[Option[A]]): Option[List[A]] = Option.traverse(s)(x => x.flatMap(Some(_)))
}
