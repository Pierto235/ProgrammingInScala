package exercism.Group2

object FoodChain {

  val lstOfAnimals = List(("fly",""),
    ("spider", "It wriggled and jiggled and tickled inside her."),
      ("bird", "How absurd to swallow a bird!"))

  def recite(start: Int, end: Int): String = {

    val lst = lstOfAnimals.take(end).reverse
    val poemsBeginning = "I know an old lady who swallowed a "
    val poemsEnd = "\nI don't know why she swallowed the fly. Perhaps she'll die.\n\n"


    def helper(acc: String, lst: List[(String, String)]): String = {
      lst match {
        case h :: Nil =>  helper(if(acc.isEmpty) acc + h._1 + "." else acc, Nil)
        case h :: tail =>  helper(acc + h._1 + ".\n" + h._2 + s"\nShe swallowed the ${h._1} to catch the ${tail.head._1}.", tail)
        case Nil => acc
      }
    }
    poemsBeginning + helper("", lst) + poemsEnd

  }

}
