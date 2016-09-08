object lists {

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List(x)
    case y :: ys => y :: init(ys)
  }

  var list1 = List(1, 2, 3, 4)
  var list2 = init(list1)
  //  assert(list1 isSameInstance list2)

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => if (n == 0) ys else y :: removeAt(n - 1, ys)
  }

  def removeAt2[T](n: Int, xs: List[T]): List[T] = xs.take(n) ::: xs.drop(n + 1)


  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case (y: List[Any]) :: ys => flatten(y) ::: flatten(ys)
    //    case y :: ys => y match {
    //      case z: List[Any] => flatten(z) ::: flatten(ys)
    //      case z => z :: flatten(ys)
    //    }
    case y :: ys => y :: flatten(ys)
  }

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if(n==0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys1) => ys1
          case (xs1, Nil) => xs1
          case (x :: xxs, z :: zs) => if(ord.lt(x,z)) x :: merge(xxs, ys) else z :: merge(xs, zs)
        }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => y * y :: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  def posFilter(xs: List[Int]):List[Int] = xs.filter(_ > 0)

  removeAt(1, List('a', 'b', 'c', 'd'))
  removeAt2(1, List('a', 'b', 'c', 'd'))
  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  msort(List(4,7,21,6,2,6,4,9))
  msort(List("pineapple", "apple", "pears","banana"))
  //  flatten2(List(List(1, 1), 2, List(3, List(5, 8))))
}

