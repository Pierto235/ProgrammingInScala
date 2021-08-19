
case class Book(title: String, authors: String *)

val books: List[Book] =
  List(
    Book("Structure and Interpretation of Computer Programs", "Abelson, Harold", "Sussman, Gerald J."),
    Book("Principles of Compiler Design", "Aho, Alfred", "Ullman, Jeffrey"),
    Book("Programming in Module-2", "Wirth, Niklaus"),
    Book("Elements of ML Programming", "Ullman, Jeffrey"),
    Book("The Java Language Specification", "Gosling, James","Joy, Bill", "Steele, Guy", "Bracha, Gilad")
  )

// Find the titles of all books whose author's last name is "Gosling"

val goslingsBooks = books.flatMap(b => b.authors.withFilter(a => a.startsWith("Gosling")).map(_ => b.title))

val goslingsBooksFor = for{
  b <- books
  a <- b.authors
  if(a.startsWith("Gosling"))
} yield b.title

// Find the titles of all books that have the string "Program" in their title

val programmingBooks = books.withFilter(p => p.title.indexOf("Program") > -1).map( b=>b.title )

val programmingBooksFor = for {
  b <- books
  t = b.title
  if(t.indexOf("Program") > -1)
} yield t

// Find the names of authors who have written at least two books

val twoBooksAuthors =
  books.flatMap(b1 =>
    books.withFilter(b2 => b1.title < b2.title).flatMap(b2=>
      b1.authors.flatMap(a1 => b2.authors.withFilter(a2 => a1 == a2).map(a2 => a1))))


val twoBooksAuthorsFor = for {
  b1 <- books
  b2 <- books
  if(b1.title < b2.title)
  a1 <- b1.authors
  a2 <- b2.authors
  if( a1 == a2)
} yield a1
