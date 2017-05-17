package recursion
package intlist

import cats._
import cats.implicits._

sealed trait IntList[A]
case class Cons[A](h: Int, tail: A) extends IntList[A]
case class Nil[A]() extends IntList[A]

object IntList {
  implicit val intListFunctor: Functor[IntList] = new Functor[IntList] {
    def map[A, B](list: IntList[A])(f: A => B): IntList[B] = list match {
      case Nil() => Nil()
      case Cons(h, t) => Cons(h, f(t))
    }
  }
}


object Playground extends App {

  val list: Fix[IntList] = Fix(Cons(1, Fix(Cons(2, Fix(Cons(3, Fix(Nil())))))))

  val sum: IntList[Int] => Int = {
    case Nil() =>
      println(s"sum 0")
      0
    case Cons(h, t) =>
      println(s"sum $h $t")
      h + t
  }

  val len: IntList[Int] => Int = {
    case Nil() => 0
    case Cons(_, t) => t + 1
  }

  val tail: IntList[(Fix[IntList], Fix[IntList])] => Fix[IntList] = {
    case Nil() => Fix(Nil())
    case Cons(h, (t, _)) => t
  }

  val small: IntList ~> IntList = new (IntList ~> IntList) {
    def apply[A](l: IntList[A]): IntList[A] = l match {
      case Nil() =>
        println("small nil")
        Nil()
      case c @ Cons(h, t) =>
        println(s"small $c")
        if(h < 10) c else Nil()
    }
  }

  val infiniteStream: Int => IntList[Int] = n => {
    println(s"infinite $n")
    Cons(n, n + 1)
  }

  val list1: Fix[IntList] = Fix(Cons(1, Fix(Cons(2, Fix(Cons(30, Fix(Nil())))))))

  val result3 = prepro(list1)(small, sum)
  println(result3)

  // val result4 = postpro(1)(small, infiniteStream)
  // println(result4)
}
