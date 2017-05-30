package recursion
package mat

import scalaz._
import scalaz.Functor
import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix

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

  val intList: List[Int] => IntList[List[Int]] = {
    case scala.collection.immutable.Nil =>
      println("intlist nil")
      Nil()
    case h :: t =>
      println(s"intlist $h $t")
      Cons(h, t)
  }

  val list: Fix[IntList] = Fix(Cons(1, Fix(Cons(2, Fix(Cons(3, Fix(Nil())))))))

  val show: IntList[(Int, String)] => String = {
    case Nil() => "Nil"
    case Cons(x, (xx, str)) =>  s"($xx, $x) :: $str"
  }

  val index: IntList[Int] => Int = {
    case Nil() => 0
    case Cons(h, t) => t + 1
  }

  val sum: IntList[Int] => Int = {
    case Nil() =>
      println("sum")
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

  // val result = cata(list)(sum)
  // println(result)
  // val result2 = para(list)(tail)
  // println(result2)

  val small: IntList ~> IntList = new (IntList ~> IntList) {
    def apply[A](l: IntList[A]): IntList[A] = l match {
      case Nil() =>
        println("nil")
        Nil()
      case c @ Cons(h, t) =>
        println(c)
        if(h < 10) c else Nil()
    }
  }


  val list1: Fix[IntList] = Fix(Cons(1, Fix(Cons(2, Fix(Cons(30, Fix(Nil())))))))
  val result = list1.prepro(small, sum)
  println(result)


 val r = list1.zygo(index, show)
  println(r)

  // val result3 = prepro(list1)(small, sum)
  // println(result3)

}
