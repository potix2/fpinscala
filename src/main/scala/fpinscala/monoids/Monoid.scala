package fpinscala.monoids
import org.scalacheck._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero: List[A] = Nil
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(gen, gen, gen) { (a:A, b:A, c:A) =>
    m.op(a,m.zero) == a && m.op(a, m.op(b, c)) == m.op(m.op(a,b), c)
  }
}
