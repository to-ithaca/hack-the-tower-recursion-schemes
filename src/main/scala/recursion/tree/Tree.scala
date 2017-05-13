package recursion
package tree

//TODO: Try and Fix this
sealed trait Tree
case class Leaf(value: Int) extends Tree
case class Node2(a: Tree, b: Tree) extends Tree
case class Node3(a: Tree, b: Tree, c: Tree) extends Tree

object Playground {

  def sum(t: Tree): Int = t match {
    case Leaf(v) => v
    case Node2(a, b) => sum(a) + sum(b)
    case Node3(a, b, c) => sum(a) + sum(b) + sum(c)
  }
  def multiply(t: Tree): Int = t match {
    case Leaf(v) => v
    case Node2(a, b) => multiply(a) * multiply(b)
    case Node3(a, b, c) => multiply(a) * multiply(b) * multiply(c)
  }
}
