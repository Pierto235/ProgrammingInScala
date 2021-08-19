
trait Queue[T]{
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

val q:Queue[Int]  = Queue(12, 23,3)
q.head
q.tail



