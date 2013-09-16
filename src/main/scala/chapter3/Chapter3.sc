import fpinscala.datastructures._

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))

// EXERCISE1
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}


List.tail(ex3)
List.setHead(ex3)("c")
List.drop(ex3,0)
List.drop(ex3,1)
List.drop(ex3,2)
