import fpinscala.laziness._

Stream.ones.take(5).toList
Stream.ones.exists(_ % 2 != 0)
