// simple example
implicit def fromStringToFirstChar(s: String) = s.toList.head

def fun(c: Char): Char = c.toUpper

fun("str")          // works because implicit conversion from String to Char
val s: Char = "str" // works because implicit conversion from String to Char

/**
There are three places implicits are used in the language:
1. Conversions to an expected type,
2. Conversions of the receiver of a selection
3. Implicit parameters
*/

