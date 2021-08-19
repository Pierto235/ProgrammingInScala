// Lower bound
class Queue[+T](val leading: List[T], val trailing: List[T]) {

  private def mirror: Queue[T] = if(!leading.isEmpty) this else new Queue(trailing.reverse, Nil)

  def head: T = mirror.leading.head
  def tail: Queue[T] = {
    val q = mirror
    new Queue[T](q.leading.tail, q.trailing)
  }
  def enqueue[U >: T](x:U): Queue[U] = new Queue[U](leading, x::trailing)

}

class Fruit   // Fruit  -  U ;  Apple - T
class Apple extends Fruit
class Orange extends Fruit

val a: Apple = new Apple
val o: Orange = new Orange

val q:Queue[Apple] = new Queue[Apple](List(a), Nil)
val q1:Queue[Fruit] = q.enqueue(o)
q.head