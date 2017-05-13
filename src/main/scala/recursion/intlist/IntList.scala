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
    case Nil() => 0
    case Cons(h, t) => h + t
  }

  val tail: IntList[(Fix[IntList], Fix[IntList])] => Fix[IntList] = {
    case Nil() => Fix(Nil())
    case Cons(h, (t, _)) => t
  }

  val result = cata(list)(sum)
  println(result)
  val result2 = para(list)(tail)
  println(result2)
}
