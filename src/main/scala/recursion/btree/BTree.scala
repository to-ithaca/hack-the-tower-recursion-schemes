package recursion
package btree

import cats._, cats.implicits._

//TODO: Try and Fix this
sealed trait Tree[A]
case class TNil[A]() extends Tree[A]
case class Leaf[A](value: Int) extends Tree[A]
case class Node[A](l: A , r: A) extends Tree[A]

object Tree {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case TNil() => TNil()
      case Leaf(v) => Leaf(v)
      case Node(l, r) => Node(f(l), f(r))
    }
  }
}

object Playground extends App {

  //there is an order for lists
  Order[List[Int]]

  //we're not using the order, but we should
  def merge(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
	  case(left, Nil) => left
	  case(Nil, right) => right
	  case(leftHead :: leftTail, rightHead :: rightTail) =>
	    if (leftHead < rightHead) leftHead::merge(leftTail, right)
        else rightHead :: merge(left, rightTail)
    }

  //builds a balanced binary tree
  val build: List[Int] => Tree[List[Int]] = {
    case Nil => TNil()
    case x :: Nil => Leaf(x)
    case xs => val (l, r) = xs.splitAt(xs.length / 2)
      Node(l, r)
  }

  //sorts a balanced binary tree
  val mergeSort: Tree[List[Int]] => List[Int] = {
    case TNil() => Nil
    case Leaf(v) => v :: Nil
    case Node(l, r) => merge(l, r)
  }

  val list = List(23, 565, 6, 23, 45, 25, 678, 5)
  val sorted = hylo(list)(mergeSort, build)
  println(sorted)
}
