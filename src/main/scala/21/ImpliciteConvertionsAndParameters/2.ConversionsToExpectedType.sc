// Example

// val i: Int = 3.5  I cant do it but:

implicit def fromDoubleToInt(x: Double) = x.toInt

val i:Int = 3.5  // it is ok now


case class Circle(r: Int)

implicit def circleToLength(c: Circle): Double = 2 * Math.PI * c.r

val l = 5

l + Circle(2)
Circle(5) + l

case class Apples(n: Int, w: Double)
case class Oranges(n: Int, w: Double)

implicit def ApplesToDouble(a: Apples): Double = a.n * a.w * 0.4
implicit def OrangesToDouble(o: Oranges): Double = o.n * o.w * 0.6

Apples(2, 0.3) + Oranges(3, 0.33)


