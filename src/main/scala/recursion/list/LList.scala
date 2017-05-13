package recursion
package list

import cats._
import cats.implicits._

sealed trait LList[+A, L]
case class Cons[A, L](h: A, tail: L) extends LList[A, L]
case class Nil[L]() extends LList[Nothing, L]

object LList {
  implicit def llistFunctor[C]: Functor[LList[C, ?]] = new Functor[LList[C, ?]] {
    def map[A, B](llist: LList[C, A])(f: A => B): LList[C, B] = llist match {
      case Nil() => Nil()
      case Cons(h, t) => Cons(h, f(t))
    }
  }
}


object Playground extends App {
  val list: Fix[LList[Int, ?]] = Fix(Cons(1, Fix(Cons(2, Fix(Cons(3, Fix(Nil())))))))

  val sum: LList[Int, Int] => Int = {
    case Nil() => 0
    case Cons(h, t) => h + t
  }

  val result = cata(list)(sum)
  println(result)
}
