package recursion
package scheme

import cats._
import cats.implicits._

trait Nat[A]
case class Zero[A]() extends Nat[A]
case class Succ[A](n: A) extends Nat[A]

object Nat {
  implicit val natFunctor: Functor[Nat] = new Functor[Nat] {
    def map[A, B](nat: Nat[A])(f: A => B): Nat[B] = nat match {
      case Zero() => Zero()
      case Succ(a) => Succ(f(a))
    }
  }
}


object Recurse extends App {
  
  val number: Fix[Nat] = Fix(Succ(Fix(Succ(Fix(Succ(Fix(Zero())))))))

  def toInt: Nat[Int] => Int = {
    case Zero() => 0
    case Succ(n) => n + 1
  }

  val result = cata(number)(toInt)
  println(result)
}
