package exercism.Group2

object House {

  private val lstOfVerses = List(
    "the house that Jack built",
    "the malt that lay in",
    "the rat that ate",
    "the cat that killed",
    "the dog that worried",
    "the cow with the crumpled horn that tossed",
    "the maiden all forlorn that milked",
    "the man all tattered and torn that kissed",
    "the priest all shaven and shorn that married",
    "the rooster that crowed in the morn that woke",
    "the farmer sowing his corn that kept",
    "the horse and the hound and the horn that belonged to"
  )

  def recite(start: Int, end: Int): String = {

    def helper(counter: Int): String =
      s"${lstOfVerses(counter)}${if (counter == 0) {"."} else {" " + helper(counter - 1)}}"

    def getVerses(vers: Int): String = {
      if(vers > end) "\n"
      else s"This is ${helper(vers-1)}\n${getVerses(vers + 1)}"
    }
    getVerses(start)
  }

}
