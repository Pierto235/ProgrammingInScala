
// Functional queue

// Queue(1,2,3,4,5,6) = Queue(List(1,2,3), List(6,5,4))

class Queue[T](val leading: List[T], val trailing: List[T]) {

  private def mirror: Queue[T] = if(!leading.isEmpty) this else new Queue(trailing.reverse, Nil)

  def head: T = mirror.leading.head
  def tail: Queue[T] = {
    val q = mirror
    new Queue[T](q.leading.tail, q.trailing)
  }
  def enqueue(x:T): Queue[T] = new Queue(leading, x::trailing)

}

/** This is hardly intuitive representation of a queue ;-) */
val q = new Queue[Int](List(1,2,3), List(6,5,4))

q.head
q.tail.leading
q.tail.tail.leading
q.tail.tail.tail.leading
q.tail.tail.tail.head
q.tail.tail.tail.tail.leading
q.enqueue(7).trailing

//1. mirror is important in the case when leading is empty, although take time to evaluate
//2. the longer queue gets the less often mirror will be called,
// because sooner or later elements from trailing will be transferred to leading