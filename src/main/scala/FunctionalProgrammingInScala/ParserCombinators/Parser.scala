package FunctionalProgrammingInScala.ParserCombinators

import scala.language.{higherKinds, implicitConversions}

object Parser extends App {

  trait Parsers0[ParseError, Parser[+_]] {
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]
    def char(c: Char): Parser[Char]

    val c = 'a'
    run(char(c))(c.toString) == Right(c)    // this parser parse a single letter 'a'


    def string(s: String): Parser[String]

    val s = "abracadabra"
    run(string(s))(s) == Right(s)           // this parser parse a string 'abracadabra'


    def orString(s1: String, s2: String): Parser[String]

    run(orString("abra", "cadabra"))("abra") == Right("abra") // this parser parse a string 'abra' or 'cadabra'


    // Lets make it more polymorphic

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    run(or(string("abra"), string("cadabra")))("abra") == Right("abra")

    run(or(char('a'), char('b')))('a'.toString)

  }

  trait Parsers[ParseError, Parser[+_]] { self =>

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asString[A](a: A)(implicit f: A => Parser[String]):ParserOps[String] = ParserOps(f(a))

    def listOfN[A](n: Int, p:Parser[A]): Parser[List[A]]

    case class ParserOps[A](p: Parser[A]) {
      def |[B >:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
      def or[B >:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    }


  }
















}
