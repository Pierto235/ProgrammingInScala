
class PreferredPrompt(val preference: String)
class PreferredDrink(val preference: String)

object Greeter {
  def greet(name: String)(implicit prompt:PreferredPrompt, drink: PreferredDrink): Unit = {
    println(s"Welcome, $name. The system is ready.")
    println(prompt.preference)
    println(s"Enjoy ${drink.preference}.")
  }
}

val bobPrompt = new PreferredPrompt("relax> ")
val bobDrink = new PreferredDrink("tea")
Greeter.greet("Bob")(bobPrompt, bobDrink)


//
object JoesPrefs {
  implicit val prompt: PreferredPrompt = new PreferredPrompt("Yes, master> ")
  implicit val drink: PreferredDrink = new PreferredDrink("tea")
}




import JoesPrefs._
Greeter.greet("Joe's")

///////////////////////////////////////////////////////////////////////

