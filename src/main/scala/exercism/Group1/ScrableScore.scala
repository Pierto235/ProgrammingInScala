package exercism.Group1

object ScrabbleScore {
  val scoreTable: Map[Char, Int] = Map(
  (Set('A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'), 1),
  (Set('D', 'G'), 2),
  (Set('B', 'C', 'M', 'P'), 3),
  (Set('F', 'H', 'V', 'W', 'Y'), 4),
  (Set('K'), 5),
  (Set('J', 'X'), 8),
  (Set('Q', 'Z'), 10)).flatMap{ case (set, point) => set.map(char => (char, point))}


  def score(text: String): Int = text.toUpperCase.toList.foldLeft(0)((a,b) => a + scoreTable(b))


//
//  val pointsTable = Map(
//    List('A', 'E', 'I','O', 'U', 'L', 'N', 'R', 'S', 'T') -> 1,
//    List('D', 'G') -> 2,
//    List('B', 'C','M', 'P') -> 3,
//    List('F', 'H', 'V', 'W','Y') -> 4,
//    List('K') -> 5,
//    List('J', 'X') -> 8,
//    List('Q', 'Z') -> 10
//  ).flatMap{ case (charList, point) => charList.map(char => char -> point) }
//
//  def score(text: String): Int =
//    text.toUpperCase.foldLeft( 0 )((a, b) => a + pointsTable(b))
//
//  def score1(text: String): Int =
//    text.toUpperCase.map(elem => pointsTable(elem)).sum


}
