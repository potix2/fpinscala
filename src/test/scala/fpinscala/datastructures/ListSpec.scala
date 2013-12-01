package fpinscala.datastructures

import org.specs2.mutable.Specification

class ListSpec extends Specification {
  val ls = Cons(1, Cons(2, Cons(3, Nil)))
  val ds = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))

  "tail" should {
    "return tail of list" in {
      List.tail(ls) must_== Cons(2, Cons(3, Nil))
    }
  }

  "setHead" should {
    "replace head of list" in {
      List.setHead(ls)(5) must_== Cons(5, Cons(2, Cons(3, Nil)))
    }
  }

  "drop" should {
    "remove two elements of list" in {
      List.drop(ls, 2) must_== Cons(3, Nil)
    }
  }

  "dropWhile" should {
    "remove elements from the List prefix as long as they match a predicate" in {
      List.dropWhile(ls)(x => x < 3) must_== Cons(3, Nil)
    }
  }

  "init" should {
    "returns a List consisting of all but the last element of a List" in {
      List.init(ls) must_== Cons(1, Cons(2, Nil))
    }
  }

  "product" should {
    "multiply the elements of the list" in {
      List.product(ds) must_== 6.0
    }
  }

  "foldRight" should {
    "construct new List when Cons and Nil are passed" in {
      List.foldRight(ls, Nil:List[Int])(Cons(_,_)) must_== Cons(1, Cons(2, Cons(3, Nil)))
    }
  }

  // exercise: 9
  "length" should {
    "return zero with Nil" in {
      List.length(Nil) must_== 0
    }
    "return the length of the list" in {
      List.length(ls) must_== 3
    }
  }

  "foldLeft" should {
    "multiply the elements of the list" in {
      List.foldLeft(ds, 1.0)(_ * _) must_== 6.0
    }
  }

  "sum3" should {
    "add the elements of the list" in {
      List.sum3(ls) must_== 6
    }
  }

  "product3" should {
    "multiply the elements of the list" in {
      List.product3(ds) must_== 6.0
    }
  }

  "reverse" should {
    "construct new list with elements in reversed order" in {
      List.reverse(ls) must_== List(3,2,1)
    }
    "return Nil when Nil is passed" in {
      List.reverse(Nil) must_== Nil
    }
  }

  "foldLeft2" should {
    "multiply the elements of the list" in {
      List.foldLeft2(ds, 1.0)(_ * _) must_== 6.0
    }
  }

  "append2" should {
    "return empty list with empty list" in {
      List.append2(List(), List()) must_== List()
    }
    "return left hand side when right hand side is empty list" in {
      List.append2(List(1,2,3), List()) must_== List(1,2,3)
    }
    "return right hand side when elft hand side is empty list" in {
      List.append2(List(), List(1,2,3)) must_== List(1,2,3)
    }
    "return right hand side when elft hand side is empty list" in {
      List.append2(List(1,2,3), List(4,5,6)) must_== List(1,2,3,4,5,6)
    }
  }

  "flatten" should {
    "return empty list with empty list" in {
      List.flatten(List()) must_== List()
    }
    "concatenate a list of lists into a single list" in {
      List.flatten(List(List(1), List(2,3))) must_== List(1,2,3)
    }
  }

  "addOne" should {
    "add one to each elements of passed list" in {
      List.addOne(List(1,2,3)) must_== List(2,3,4)
    }
  }

  "doubleToString" should {
    "convert double elements to string" in {
      List.doubleToString(List(1.0,2.0,3.0)) must_== List("1.0", "2.0", "3.0")
    }
  }

  "map" should {
    "apply f to each elements of passed list" in {
      List.map(List(1.0,2.0,3.0))(_.toString()) must_== List("1.0", "2.0", "3.0")
    }
  }

  "filter" should {
    "find the elements that matches predicate" in {
      List.filter(List(1,2,3,4,5,6))(_ % 2 == 0) must_== List(2,4,6)
    }
  }

  "flatMap" should {
    "aa" in {
      List.flatMap(List(1,2,3))(i => List(i,i)) must_== List(1,1,2,2,3,3)
    }
  }

}
