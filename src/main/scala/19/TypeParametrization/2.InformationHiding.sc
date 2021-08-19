
/** This is hardly intuitive representation of a queue ;-) */
//val q = new Queue[Int](List(1,2,3), List(6,5,4))

// We can make the constructor private to hide this incomfortable constructor to the client
//  make constructor private and add some auxiliary constructor inside the class

class Queue[T] private (val leading: List[T], val trailing: List[T]) {

  private def mirror: Queue[T] = if(!leading.isEmpty) this else new Queue(trailing.reverse, Nil)

  def head: T = mirror.leading.head
  def tail: Queue[T] = {
    val q = mirror
    new Queue[T](q.leading.tail, q.trailing)
  }
  def enqueue(x:T): Queue[T] = new Queue(leading, x::trailing)

  def this() = this(Nil, Nil)
  def this(lst: T*) = this(lst.toList, Nil)

}

val q = new Queue[Int]()
val q1 = new Queue[Int](1,2,3)

def fun():Queue[Int] = ???

// --------------------------------------------------------

// Or we can crete companion object

object Queue {
  def apply[T](lst: T*) = new Queue(lst.toList, Nil)
}

val q2 = Queue(1,2,3)