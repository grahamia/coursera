import scala.annotation.tailrec

object exercise2 {

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    @tailrec def loop(a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1, acc+f(a))
    }
    loop(a, 0)
  }

  sum((x) => 2 * x)(1,10)

  def mapReduce(map: Int => Int, reduce: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if(a>b) zero
    else reduce(map(a), mapReduce(map, reduce, zero)(a + 1, b))
  }

  def product(f: Int => Int)(a: Int, b:Int): Int = mapReduce(f, (x,y) => x * y, 1)(a,b)

  product(x => x * x)(3,4)

  def factorial(n: Int) = product(x => x)(1, n)

  factorial(5)

}
