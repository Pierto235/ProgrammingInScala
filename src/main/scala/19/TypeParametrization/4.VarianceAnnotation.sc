class Cell[T](init: T) {
  private[this] var current = init
  def get: T = current
  def set(x: T): Unit = { current = x } // by adding +T I will get covariant type T occurs
                                        // in contravariant position in type T of value x
}

val c1 = new Cell[String]("abc") // c1 = "abc"
val c2: Cell[Any] = c1    // since nonvariant by default
c2.set(1)
val s: String = c1.get

// covariance        S < T => Cell[S] < Cell[T]
// contravariance    S < T => Cell[S] > Cell[S]
// Any(w tym wypadku 1) przypisuje do String w def set czyli musiałbym mieć contravariance,
// ale mam covariance