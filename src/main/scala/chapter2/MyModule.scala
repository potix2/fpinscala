package chapter2

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst[A](ds: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ds.length) -1
      else if (p(ds(n))) n
      else loop(n + 1)

    loop(0)
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n *acc)

    go(n,1)
  }

  //EXERCISE1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int = {
      if (n == 0) acc1
      else if (n == 1) acc2
      else go(n-1, acc2, acc1 + acc2)
    }

    go(n, 0, 1)
  }

  //EXERCISE2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int):Boolean = {
      if (n < 1) true
      else gt(as(n - 1), as(n)) && go(n - 1)
    }

    go(as.length - 1)
  }

  //EXERCISE3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b)

  //EXERCISE4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a:A, b:B) => f(a)(b)

  //EXERCISE5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  }
}
