package fpinscala.datastructures

trait TestHelper {
  def doAssert(msg: String, lhs: Any, rhs: Any) {
    println(s"${msg}: Comparing ${lhs} == ${rhs}")
    assert(lhs == rhs, s"${lhs} is not equal to ${rhs}")
  }
}

object ListRunner extends App with TestHelper {
  import List._
  
  println("running")
  
  val xs = List(1, 2, 3)
  
  doAssert("tail(xs)", tail(xs), List(2, 3))
  doAssert("setHead(xs, 0)", setHead(xs, 0), List(0, 2, 3))
  doAssert("drop(xs, 2)", drop(xs, 2), List(3))
  
  val ys = List(5, 4, 3, 2, 1)
  val gt3: Int => Boolean = (x: Int) => ( x > 3 )  
  def `gt3'`(x: Int): Boolean = ( x > 3 )
  
  doAssert("dropWhile(ys, gt3)", dropWhile(ys, gt3), List(3, 2, 1))
  doAssert("dropWhile(ys, gt3')", dropWhile(ys, `gt3'`), List(3, 2, 1))
  
  doAssert("init(xs)", init(xs), List(1, 2))
  
  doAssert("length(xs)", length(xs), 3)
  val fl = foldLeft(xs, 0)((el, acc) => { acc + el })
  doAssert("foldLeft(xs, 0)((el, acc) => { acc + el })", fl, 6)
  
  val mp = map(xs)(_ * 2)
  doAssert("map(xs)(_ * 2)", mp, List(2, 4, 6))
  
}