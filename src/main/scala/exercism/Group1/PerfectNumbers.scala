package exercism.Group1

object NumberType extends Enumeration {
  type NumberType = Value
  val Perfect, Abundant, Deficient = Value
}

import NumberType._

object PerfectNumbers {

  def findEliquotSum(n: Int): Int =
    (1 to n/2).foldLeft(0)((a,b) => if(n%b == 0) a + b else a )

  def classify(num: Int): Either[String, NumberType] = {
    if (num > 0) {
      findEliquotSum(num) match {
        case s if s == num => Right(NumberType.Perfect)
        case s if s < num => Right(NumberType.Deficient)
        case s if s > num => Right(NumberType.Abundant)
      }
    } else Left("Classification is only possible for natural numbers.")

  }

}

//object NumberType extends Enumeration {
//  type NumberType = Value
//  val Perfect, Abundant, Deficient = Value
//}
//
//import NumberType._
//
//object PerfectNumbers {
//
//  def getAliquotSum(n: Int): Int=
//    (1 to n/2).flatMap(el => if(n%el == 0) Some(el) else None).sum
//
//  def classify(number: Int): Either[String, NumberType] = {
//
//    if(number < 1) Left("Classification is only possible for natural numbers.")
//    else {
//      number - getAliquotSum(number) match {
//        case 0 => Right(NumberType.Perfect)
//        case l if l < 0 => Right(NumberType.Abundant)
//        case l if l > 0 => Right(NumberType.Deficient)
//      }
//    }
//
//  }
//
//}
