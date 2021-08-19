// intro
val lst0 = List("a", "b", "c")
val lst1 = List(1,2,3)

for {
  s <- lst0
  n <- lst1
} yield (s,n)

//filtering
case class Person(name: String, isMale: Boolean, children: Person*)

val lara = Person("Lara", false)
val bob = Person("Bob", true)
val julie = Person("Julie", false , lara, bob)

val persons = List(lara, bob, julie)

persons.filter(p => !p.isMale).flatMap(f => f.children.map(c => (f.name, c.name)))
persons.withFilter(p => !p.isMale).flatMap(f => f.children.map(c => (f.name, c.name)))

val pairs = for {
  p <- persons
  if(!p.isMale)
  c <- p.children
} yield (p.name, c.name )

/** For expression syntax */
//  for(seq) yield expr
// seq = generator, definition, filter
// for( p <- persons, n = p.name, if(s startsWith "To"))

