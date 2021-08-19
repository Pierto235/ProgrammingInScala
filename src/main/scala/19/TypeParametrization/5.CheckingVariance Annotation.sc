// Engeneering an unsound example for no reassignable fields
trait Queue[+T]{
  def head: T
  def tail: Queue[T]
  def enqueue(x: T): Queue[T]
}

object Queue {

  def apply[T](lst: T*):Queue[T] = new QueueImpl[T](lst.toList, Nil)

  private class QueueImpl[T](val leading: List[T], val trailing: List[T]) extends Queue[T] {
    def mirror = if (!leading.isEmpty) this else new QueueImpl(trailing.reverse, Nil)
    def head = mirror.leading.head
    def tail:Queue[T] = new QueueImpl(mirror.leading.tail, trailing)
    def enqueue(x: T) = new QueueImpl(leading, x::trailing)
  }
}

class StrangeIntQueue extends Queue[Int] {
  override def enqueue(x: Int): Queue[Int] = {
    println(math.sqrt(x))
    super.enqueue(x)
  }
}

val x: Queue[Any] = new StrangeIntQueue
x.enqueue("abc")
// I try to make sqrt for "abc"
