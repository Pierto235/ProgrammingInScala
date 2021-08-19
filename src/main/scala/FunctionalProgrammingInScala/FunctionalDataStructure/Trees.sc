// Tree
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(n) => n
      case Branch(l,r) => maximum(l) max maximum(r)
    }

}

val t = Branch(Leaf(4), Branch(Leaf(6), Leaf(1)))

val s = Tree.size(t)
val m = Tree.maximum(t)

Tree.size(Leaf(1))
Tree.size(Branch(Leaf(1), Leaf(2)))


