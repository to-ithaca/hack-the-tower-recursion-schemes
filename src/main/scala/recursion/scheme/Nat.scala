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


  val toNat: Int => Nat[Int] = {
    case 0 => Zero()
    case n => Succ(n - 1)  
  }

  val toInt: Nat[Int] => Int = {
    case Zero() =>
      0
    case Succ(n) =>
      n + 1
  }

  val factorial: Nat[(Fix[Nat], Int)] => Int = {
    case Zero() =>
      println("1")
      1
    case Succ((n, i)) =>
      println(n)
      (cata(n)(toInt) + 1) * i
  }

  val pred: Nat[(Fix[Nat], Fix[Nat])] => Fix[Nat] = {
    case Zero() =>
      println("zero")
      Fix(Zero())
    case Succ((p, n)) =>
      println("succ")
      //println(s"succ ${cata(p)(toInt)} ${cata(n)(toInt)}")
      p
  }

 // val result = cata(number)(toInt)
  // println(result)
  // val result2 = ana(5)(toNat)
  // println(result2)
  // val result3 = hylo(5)(toInt, toNat)
  // println(result3)


  val four = ana(4)(toNat)
  val result4 = para(four)(factorial)
  // println(result4)
  //println(cata(para(ana(5)(toNat))(pred))(toInt))
}
