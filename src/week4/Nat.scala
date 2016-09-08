package week4

// peano numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new IllegalStateException("No")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(that.isZero) that else throw new IllegalStateException("No")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}

object List {
  def List[T](x: T): List[T] = List(x)
  def List[T](x1: T, x2: T): List[T] = ???
  def List[T](x1: T, x2: T, x3: T): List[T] = ???
}

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
  def show: String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => s"${e1.show} + ${e2.show}"
    case Prod(e1, e2) =>  {
      def innerProd(inner: Expr): String = inner match {
        case Sum(_, _) => "(" + inner.show + ")"
        case _ => inner.show
      }
      innerProd(e1) + " * " + innerProd(e2)
    }
    case Var(x) => x
  }
}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(x: String) extends Expr
