object listfun {
  val nums = List(2,-4,5,7,1)
  val fruit = List("apple", "pineapple", "orange", "banana")

  def par(i: Int): Boolean = i>0
  nums filter par
  nums filterNot par
  nums partition par
  nums takeWhile par
  nums dropWhile par
  nums span par

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (left, right) = xs span(_==x)
      left :: pack(right)
  }

  pack(List("a", "a", "a", "b", "c", "c", "a"))

  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map { x =>
      (x.head, x.size)
    }
  }

  encode(List("a", "a", "a", "b", "c", "c", "a"))

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_ :: _)

  concat(List(2,5,7,4), List(34,35,23,342))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)

  def mapFun2[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldLeft List[U]())((ys: List[U], y:T) => ys :+ f(y))

  def mapFun5[T, U](xs: List[T], p: T => U): List[U] =
    (xs foldLeft List[U]())(_ :+ p(_) )

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_, n: Int) => n + 1)

  def lengthFun2[T](xs: List[T]): Int =
    (xs foldLeft 0)((x, _) => x + 1)

  mapFun(List(1,2,4,5,56,6), (x:Int) => x + 1)
  mapFun2(List(1,2,4,5,56,6), (x:Int) => x + 1)
  mapFun5(List(1,2,4,5,56,6), (x:Int) => x + 1)
  lengthFun(List(1,2,4,5,66,6))
  lengthFun2(List(1,2,4,5,66,6))

  val sentence = List("Mary", "had", "a", "little", "lamb")
  sentence.foldRight("start")((a,b) => {
    println("[a:" + a + "][b:"+ b + "]");
    a + b
  })
}