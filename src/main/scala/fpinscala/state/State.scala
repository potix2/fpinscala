package fpinscala.state

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

}
