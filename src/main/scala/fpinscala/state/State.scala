package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, ss) = run(s)
      g(a).run(ss)
    })

  def map[B](f: A => B): State[S,B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](rb: State[S,B])(f: (A, B) => C): State[S,C] =
    for {
      a <- this
      b <- rb
    } yield(f(a,b))

}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  def positiveEven: Rand[Int] =
    map(positiveInt)(i => i - i % 2)

  /**
   * exercise1
   */
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
   * exercise2
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = positiveInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /**
   * exercise3
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  /**
   * exercise4
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(acc: List[Int])(c: Int)(r: RNG): (List[Int], RNG) =
      if ( c == 0 )
        (acc, r)
      else {
        val (i, rr) = r.nextInt
        go(i :: acc)(c - 1)(rr)
      }
    go(Nil)(count)(rng)
  }

  /**
   * exercise5
   */
  def double_1: Rand[Double] =
    map(positiveInt)(i => i / (Int.MaxValue.toDouble + 1))

  /**
   * exercise6
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra,rb)((_,_))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double_1)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double_1, int)

  /**
   * exercise7
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(RNG.unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  /**
   * exercise8
   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def positiveLessThan(n: Int): Rand[Int] =
    flatMap(positiveInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod > 0) unit(mod) else positiveLessThan(n)
    }

  /**
   * exercise9
   */
  def map_1[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_1[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb)(b => f(a,b))
    }
}

object State {
  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))

  def traverse[S,A,B](fs: List[State[S, A]])(f: A => B): State[S, List[B]] =
    fs.foldRight(unit(List()): State[S, List[B]])((s, acc) => s.map2(acc)(f(_) :: _))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    traverse(fs)(identity)

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S,S] = State(s => (s,s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(i => State.modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    })))
    s <- State.get
  } yield(s.coins, s.candies)
}
