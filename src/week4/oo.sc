import week4._

//package week4

//object Thingy {

  val badger = new Succ(Zero)

  badger.successor



Sum(Prod(Number(2), Var("x")), Var("y")).show
Sum(Prod(Sum(Number(2), Var("x")), Var("y")),Prod(Sum(Number(2), Var("x")), Var("y"))).show

def insert(x: Int, xs: scala.List[Int]): scala.List[Int] = xs match {
  case scala.List() => scala.List(x)
  case y :: ys => if(x<=y) x::y::ys else y :: insert(x, ys)
}

insert(5, scala.List(3,4,6,7,8))
//}
