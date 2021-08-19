import java.time.LocalDate

case class Rational(n: Int, d: Int) {
  def + (that: Rational) = Rational(n * that.d + that.n * d, d * that.d)
  def + (that: Int) = Rational(n+ (d * that), d)

  def **(i: Int):Int = i * i
}

// interoperating with new types
implicit def intToRational(n: Int) = Rational(n, 1)

val r = Rational(1,2)
r + r
r + 2
2 + r

//simulating new syntax
implicit def anyStringToSquaredInt(x: String) = Rational(1,1)

"x" ** 5
"adf" ** 10


////////////////////////////////////////////////////
// implicit classes

implicit class ImprovedLocalDate(d: LocalDate){
  def isBeforeOrEqual(that: LocalDate): Boolean = d.isEqual(that) || d.isBefore(that)
  def isAfterOrEqual(that: LocalDate): Boolean = d.isEqual(that) || d.isAfter(that)
}

val date = LocalDate.now()
date.isBeforeOrEqual(LocalDate.now())
date.isAfterOrEqual(LocalDate.now().minusDays(1))


//--------------------------------------------------------
case class Rectangle(width: Int, height: Int)

implicit class RectangleMaker(width: Int){
  def x(height: Int) = Rectangle(width, height)
}

3.x(4)
5 x 7

//-----------------------------------------------------------

case class Computers(price: Double, n: Int, model: String = "i9 Intel"){
  override def toString: String =
    s"You have bought ${n} items of ${model}, and you pay ${price * n} pounds."
}

implicit class ReceiptMaker(n: Int){
  def price(p: Double) = Computers(p, n)
}

5 price 390


















