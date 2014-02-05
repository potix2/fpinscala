package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  /**
   * exercise1
   */
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i < 0)
      (Int.MinValue, rng2)
    else
      (i, rng2)
  }

  /**
   * exercise2
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = rng.nextInt
    (i.toDouble / Int.MaxValue, rng2)
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
}
