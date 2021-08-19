                    // VALUE OBJECT DESIGN PATTERN

/** In programming, there are different ways of comparing data.
 * We can compare object identities or their values.
 * These are useful in different scenarios and here,
 * we will see what value objects are and when they can be used.
 *
 * Value objects are used to represent numbers, money, dates, and so on.
 * They should be small and immutable;
 *
 *  They are quite useful in:
 *  1. multithreaded applications due to their immutability.
 *  2. data transfer objects in enterprise applications.
 *  3. there are no other major drawbacks to using this pattern.
 *
 * *******************************************************************************************************
 *
 * Case classes and tuples are immutable and they are used to achieve the Value Object Design Pattern.
 *
 * ********************************************************************************************************
 * */

/////////////////////////////////////////////////////////////////////////////////////////////////////////

// Code Example

// This is everything we need in order to get a value object.
// Scala does everything for us in the background by creating default implementations for the hashCode, equals, and toString methods

case class Date(
  day: Int,
  month: String,
  year: Int )

val thirdOfMarch = Date(3, "MARCH", 2016)
val fourthOfJuly = Date(4, "JULY", 2016)
val newYear1 = Date(31, "DECEMBER", 2015)
val newYear2 = Date(31, "DECEMBER", 2015)

println(s"The 3rd of March 2016 is the same as the 4th of July 2016: ${thirdOfMarch == fourthOfJuly}")
println(s"The new year of 2015 is here twice: ${newYear1 == newYear2}")

//the same result could be achieved using tuples like:
val date1 = (31, "DECEMBER", 2015)
val date2 = (31, "DECEMBER", 2015)
date1 == date2

/////////////////////////////////////////////////////////////////////////////////////////////
// Bad design
class BadDate(
 day: Int,
 month: String,
 year: Int
)

val thirdOfMarch1 = new BadDate(3, "MARCH", 2016)
val fourthOfJuly1 = new BadDate(4, "JULY", 2016)
val newYear11 = new BadDate(31, "DECEMBER", 2015)
val newYear21 = new BadDate(31, "DECEMBER", 2015)
println(s"The 3rd of March 2016 is the same as the 4th of July 2016: ${thirdOfMarch1 == fourthOfJuly1}")
println(s"The new year of 2015 is here twice: ${newYear11 == newYear21}")

// The reason for the preceding result is that classes, by default,
// are compared with each other by their reference identities and not by the values they carry.