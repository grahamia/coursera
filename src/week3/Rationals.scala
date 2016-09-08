package week3

class Rational(a: Int, b: Int) {
  require(b!=0, "Stop being stupid")
  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a % b)
  def numer = a
  def denom = b

  def this(a: Int) = this(a, 1)

  def +(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def -(that: Rational): Rational = this + -that

  def less(that: Rational): Boolean = numer * that.denom < that.numer * denom

  def max(that: Rational): Rational = if(this.less(that)) that else this

  override def toString = {
    val g = gcd(numer, denom)
    s"${numer/g}/${denom/g}"
  }
}