package bonkers

import org.scalatest.{FreeSpec, FunSpec, FunSuite, PropSpec, WordSpec}
import org.scalatest.Matchers._
import org.scalatest.refspec.RefSpec

object ScalaTestingStyles

class CalculatorSuite extends FunSuite {
  val calculator = new Calculator

  test("multiplication by 0 should always be 0") {
    assert(calculator.multiply(42134,0) == 0)
    assert(calculator.multiply(-42134,0) == 0)
    assert(calculator.multiply(0,0) == 0)
  }

  test("divide by 0 should throw some math error"){
    assertThrows[ArithmeticException](calculator.divide(43124, 0))
  }

  test("should return correct value for regular value"){
    calculator.add(3,6) should be (9)
    calculator.subtract(3,6) should be (-3)
    calculator.multiply(3,6) should be (18)
    calculator.divide(6,2) should be (3)
  }

}

// Behaviour Driven Development
class CalculatorSpec extends FunSpec {

  val calculator = new Calculator

  describe("multiplication"){
    describe("some other tests"){
      it("should give back 0 if multiplied by 0"){
        assert(calculator.multiply(42134,0) == 0)
        assert(calculator.multiply(-42134,0) == 0)
        assert(calculator.multiply(0,0) == 0)
      }
    }
  }

  describe("division"){
    it("should throw a math error if dividing by 0"){
      assertThrows[ArithmeticException](calculator.divide(43124, 0))
    }
  }

}

class CalculatorWordSpec extends WordSpec {
  val calculator = new Calculator

  "A calculator" should {
    "give back 0 if multiplying by 0" in {
      assert(calculator.multiply(42134,0) == 0)
      assert(calculator.multiply(-42134,0) == 0)
      assert(calculator.multiply(0,0) == 0)
    }

    "throw a math error if dividing by 0" in {
      assertThrows[ArithmeticException](calculator.divide(43124, 0))
    }
  }
}

// The most flexible testing style
class CalculatorFreeSpec extends  FreeSpec {
  val calculator = new Calculator

  "A calculator" - {// anything you want
    "give back 0 if multiplying by 0" in {
      assert(calculator.multiply(42134,0) == 0)
      assert(calculator.multiply(-42134,0) == 0)
      assert(calculator.multiply(0,0) == 0)
    }

    "throw a math error if dividing by 0" in {
      assertThrows[ArithmeticException](calculator.divide(43124, 0))
    }
  }
}

// Property - style checking
//library "scala check" generates testing examples automatically

class CalculatorPropSpec extends PropSpec {
  val calculator = new Calculator

  val multiplyByZeroExamples = List((4234,0), (-4324,0), (0,0))

  property("Calculator multiply by 0 should be 0"){
    assert(multiplyByZeroExamples.forall{
      case (a,b) => calculator.multiply(a,b) == 0
    })
  }

  property("calculator divide by 0 should throw some math error"){
    assertThrows[ArithmeticException](calculator.divide(43124, 0))
  }

}


class CalculatorRefSpec extends RefSpec { // based on reflection
  object `A claculator`{
    //test suite
    val calculator = new Calculator

    def `multiply by 0 should be 0` = {
      assert(calculator.multiply(42134,0) == 0)
      assert(calculator.multiply(-42134,0) == 0)
      assert(calculator.multiply(0,0) == 0)
    }

    def ` should throw a math error when dividing by 0` = {
      assertThrows[ArithmeticException](calculator.divide(43124, 0))
    }

  }
}

class Calculator {
  def add(a: Int, b: Int): Int = a + b
  def subtract(a: Int, b: Int): Int = a - b
  def multiply(a: Int, b: Int): Int = a * b
  def divide(a: Int, b: Int): Int = a / b



}
