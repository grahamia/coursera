package week3

import java.util.NoSuchElementException

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(i: Int): T
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)


}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
  def nth(i: Int): T = {
    def nth(i: Int, cons: List[T]): T = {
      if (i<0||cons.isEmpty) throw new IndexOutOfBoundsException
      else if (i == 0) cons.head
      else nth(i - 1, cons.tail)
    }
    if(i==0) head else nth(i - 1, tail)
  }

}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def nth(i: Int): Nothing = throw new IndexOutOfBoundsException
}

object Badger extends App {
  val list = new Cons(1, new Cons(2, new Cons(3, Nil)))
//  println(list nth 0)
//  println(list nth 1)
//  println(list nth 2)
  println(list nth 3)

  val b: List[String] = Nil

  def f(xs: List[NonEmpty], x: Empty): List[IntSet] = xs prepend x
}

object List {
  // List(1,2) = List.apply(1,2)
  def apply[T]() = Nil
  def apply[T](x: T) = new Cons(x, Nil)
  def apply[T](x1: T, x2: T) = new Cons(x1, new Cons(x2, Nil))
}