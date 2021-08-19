// queue

class Queue[T](val leading:List[T], val trailing: List[T]){
  private def mirror =
    if (!leading.isEmpty) this else new Queue[T](trailing.reverse, Nil)
  def head: T = mirror.leading.head
  def tail: Queue[T] = {
    val q = mirror
    new Queue[T](q.leading.tail, q.trailing )
  }
  def enqueue(x: T) = new Queue[T](leading, x::trailing)
}

val q = new Queue[Int](List(1,2,3), List(6,5,4))
q.leading
q.trailing
q.tail.leading
q.tail.leading.head
q.enqueue(7).trailing

val q1 = new Queue[Int](List(1,2,3,4,5), Nil)

q.tail.tail.tail.leading
q.tail.tail.tail.head
q.tail.tail.tail.tail.leading
q.tail.tail.tail.tail.head

//

class Queue2[T] private (private val leading: List[T], private val trailing: List[T]) {
  private def mirror:Queue2[T] =
    if (!leading.isEmpty) this else new Queue2[T](trailing.reverse, Nil)
  def head: T = mirror.leading.head
  def tail: Queue2[T] = {
    val q = mirror
    new Queue2[T](q.leading.tail, q.trailing )
  }
  def enqueue(x: T) = new Queue[T](leading, x::trailing)
  
  def this(lst: List[T]) = this(lst, Nil)
  
}

val q3 = new Queue2[Int](List(1,2,3,6,5,4))
val q4 = new Queue2[Int](List())

//


class Queue3[T] private (private val leading: List[T], private val trailing: List[T]) {
  private def mirror:Queue3[T] =
    if (!leading.isEmpty) this else new Queue3[T](trailing.reverse, Nil)
  def head: T = mirror.leading.head
  def tail: Queue3[T] = {
    val q = mirror
    new Queue3[T](q.leading.tail, q.trailing )
  }
  def enqueue(x: T) = new Queue[T](leading, x::trailing)
}
object Queue3 {
  def apply[T](elem:T*) = new Queue3[T](elem.toList, Nil)
}

val q5 = Queue3[Int](1,2,3,4,5,6)
val q6 = Queue3[Int]()

//


trait Queue4[T] {
  def head: T
  def tail: Queue4[T]
  def enqueue(x: T): Queue4[T]
}

object Queue4 {
  def apply[T](elems: T*): Queue4[T] = new Queue4Impl[T](elems.toList, Nil)

  private class Queue4Impl[T](private val leading: List[T],
                              private val trailing: List[T]) extends Queue4[T]{
    private def mirror:Queue4Impl[T] =
      if (!leading.isEmpty) this else new Queue4Impl[T](trailing.reverse, Nil)
    def head: T = mirror.leading.head
    def tail: Queue4Impl[T] = {
      val q = mirror
      new Queue4Impl[T](q.leading.tail, q.trailing )
    }
    def enqueue(x: T) = new Queue4Impl[T](leading, x::trailing)
  }
}

val q5 = Queue4[Int](1,2,3,4,5,6)
val q6 = Queue4[Int]()
val a: Int = 4
val q7: Queue3[Any] = Queue3(1,2,3)
q7.enqueue(a)


//


class Cell[T](init: T) {
  private[this] var current = init
  def get: T = current
  def set(x: T): Unit = current = x
}

val c1 = new Cell[String]("abc")
val c2: Cell[Any] = c1
c2.set(1)
val s: String = c1.get






































