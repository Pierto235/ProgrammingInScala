
/** Translation of for-expression  */

case class Book(title: String, authors: String *)

val books: List[Book] =
  List(
    Book("Structure and Interpretation of Computer Programs", "Abelson, Harold", "Sussman, Gerald J."),
    Book("Principles of Compiler Design", "Aho, Alfred", "Ullman, Jeffrey"),
    Book("Programming in Module-2", "Wirth, Niklaus"),
    Book("Elements of ML Programming", "Ullman, Jeffrey"),
    Book("The Java Language Specification", "Gosling, James","Joy, Bill", "Steele, Guy", "Bracha, Gilad")
  )


//1. TRANSLATING FOR EXPRESSION WITH ONE GENERATOR
/**
  for ( x <- expr1 ) yield expr2
  expr1.map( x => expr2 )
 */
for {
  b <- books
} yield b.title

books.map(b => b.title )


//2. TRANSLATING FOR EXPRESSION STARTING WITH ONE GENERATOR AND FILTER
/**
 * for ( x <- expr1  if(expr2);  seq  ) yield expr3
 * for ( x <- expr1 withFilter ( x => expr2 );  seq  ) yield expr3
 * expr1.withFilter( x => expr2 ).map( x => expr3 )
 */
// Find the titles of all books that have the string "Program" in their title

for {
  b <- books
  if(b.title.indexOf("Program") > -1)
} yield b.title

for {
  x <- books.withFilter(x => x.title.indexOf("Program") > -1)
} yield x.title

books.withFilter(p => p.title.indexOf("Program") > -1).map( b=>b.title )


//3. TRANSLATING FOR EXPRESSION STARTING WITH TWO GENERATORS

/**
 * for ( x <- expr1 ; y <- expr2;  seq ) yield expr3
 * expr1.flatMap( x => for ( y <- expr2 ; seq ) ) yield expr3
 * expr1.flatMap( x => expr2.map( y => expr3))
 *
 */

val lst1 = List(1,2,3,4)
val lst2 = List("a", "b", "c", "d")

for( n <- lst1; l <- lst2) yield s"$n-$l"
lst1.flatMap(n => lst2.map(l => s"$n-$l"))


//4. TRANSLATING PATTERNS IN GENERATORS
/**
  for ((x1, x2, ..., xn) <- expr1) yield expr2
  expr1.map { case (x1, x2, ..., xn) => expr2  }
 */

val lstOfTuples:List[(Int, Int, Int)] = List((1,2,3), (1,2,4), (4,5,6), (1,2,3))

for { (x1,x2,x3) <- lstOfTuples } yield (x2, x3)
lstOfTuples.map{ case (x1, x2, x3) => (x2, x3) }


//5. TRANSLATING DEFINITIONS

/**
  for (x <- expr1; y = expr2; seq) yield expr3
  for ((x,y) <- for ( x <- expr1 ) yield (x, expr2); seq ) yield expr3
*/


//6. TRANSLATING FOR-LOOPS

/**
  for ( x <- expr1 ) body
  expr1.forEach( x => body )
 */

/**
 * for ( x <- expr1  if(expr2);  y <- expr3  ) body
 * expr1.filterWith( x => expr2) expr3.forEach( y => body )

 */















