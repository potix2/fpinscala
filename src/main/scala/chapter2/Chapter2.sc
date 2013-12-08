import chapter2.MyModule._

//List(0,1,2,3,4,5,6,7,8,9).map(fib).foreach(println)
findFirst(Array(7, 9, 13), (x: Int) => x == 9)

isSorted(Array(), (x:Int, y:Int) => x <= y)
isSorted(Array(1), (x:Int, y:Int) => x <= y)
isSorted(Array(1,2), (x:Int, y:Int) => x <= y)
isSorted(Array(1,2,3), (x:Int, y:Int) => x <= y)
isSorted(Array(1,1,2), (x:Int, y:Int) => x <= y)
isSorted(Array(3,2,1), (x:Int, y:Int) => x <= y)


val curriedF = curry((x:Int, y:Int) => x + y)
curriedF(2)
curriedF(2)(3)
uncurry(curriedF)(2,3)

val square = (x: Int) => x * x
val addOne = (y: Int) => y + 1
compose(square, addOne)(2)

