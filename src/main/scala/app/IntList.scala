// package app

// import scalaz._
// import scalaz.Functor
// import scalaz.Free
// import matryoshka._
// import matryoshka.data._
// import matryoshka.patterns.EnvT
// import matryoshka.implicits._
// import Scalaz._

// sealed trait IntList[A]
// case class Cons[A](h: Int, tail: A) extends IntList[A]
// case class Nil[A]() extends IntList[A]

// object IntList {
//   implicit val intListFunctor: Functor[IntList] = new Functor[IntList] {
//     def map[A, B](list: IntList[A])(f: A => B): IntList[B] = list match {
//       case Nil() => Nil()
//       case Cons(h, t) => Cons(h, f(t))
//     }
//   }
// }


// object IntListPlayground extends App {

//   val list: Fix[IntList] = Fix(Cons(1, Fix(Cons(2, Fix(Cons(3, Fix(Nil())))))))

//   def build: List[Int] => IntList[List[Int]] = {
//     case scala.collection.immutable.Nil => Nil()
//     case x :: xs => Cons(x, xs)
//   }

//   val sum: IntList[Int] => Int = {
//     case Nil() =>
//       println(s"sum 0")
//       0
//     case Cons(h, t) =>
//       println(s"sum $h $t")
//       h + t
//   }

//   val len: IntList[Int] => Int = {
//     case Nil() => 0
//     case Cons(_, t) => t + 1
//   }

//   //this doesn't work - how does cofree help?
//   // val odd: IntList[Fix[IntList]] => Fix[IntList] = {
//   //   case Nil() =>
//   //     println("start nil")
//   //     Fix(Nil())
//   //   case c @ Cons(h, Fix(Nil())) =>
//   //     println(s"keep $c")
//   //     Fix(c)
//   //   case c @ Cons(h, Fix(Cons(_, t))) =>
//   //     println(s"skip $c")
//   //     Fix(Cons(h, t))
//   // }


//   val odd: IntList[Cofree[IntList, Fix[IntList]]] => Fix[IntList] = {
//     case Nil() =>
//       println("start nil")
//       Fix(Nil())
//     case c @ Cons(h, Cofree((m, Nil()))) =>
//       println(s"keep $c")
//       Fix(Cons(h, m))
//     case c @ Cons(h, Cofree((m, Cons(_, Cofree(mm, _))))) =>
//       println(s"skip $c")
//       Fix(Cons(h, mm))
//   }

//   val tail: IntList[(Fix[IntList], Fix[IntList])] => Fix[IntList] = {
//     case Nil() => Fix(Nil())
//     case Cons(h, (t, _)) => t
//   }

//   val small: IntList ~> IntList = new (IntList ~> IntList) {
//     def apply[A](l: IntList[A]): IntList[A] = l match {
//       case Nil() =>
//         println("small nil")
//         Nil()
//       case c @ Cons(h, t) =>
//         println(s"small $c")
//         if(h < 10) c else Nil()
//     }
//   }

//   def mapHead(f: Int => Int): Fix[IntList] => IntList[\/[Fix[IntList], Fix[IntList]]] = {
//     case Fix(Nil()) => Nil()
//     case Fix(Cons(h, t)) => Cons(f(h), \/.left(t))
//   }

//   def knockback: Fix[IntList] => IntList[\/[Fix[IntList], Fix[IntList]]] = {
//     case Fix(Nil()) =>
//       println(s"knockback nil")
//       Nil()
//     case Fix(Cons(x, xs)) => xs match {
//       case Fix(Cons(h, t)) =>
//         println(s"knockback $x <= $h")
//         if(x <= h) Cons(x, \/.left(xs))
//         else Cons(h, \/.right(Fix(Cons(x, t))))
//       case Fix(Nil()) =>
//         println("knockback fix nil")
//         Cons(x, \/.left(xs))
//     }
//   }

//   def knockbackApo(l: Fix[IntList]): Fix[IntList] = l.apo[Fix[IntList]](knockback)
//   def insersionSort: IntList[Fix[IntList]] => Fix[IntList] = l => {
//     println(s"insertion sort $l")
//     knockbackApo(Fix(l))
//   }


//   val infiniteStream: Int => IntList[Int] = n => {
//     println(s"infinite $n")
//     Cons(n, n + 1)
//   }

//   val list1: Fix[IntList] = Fix(Cons(30, Fix(Cons(31, Fix(Cons(20, Fix(Nil())))))))

//   // val r = list1.prepro(small, sum)
//   // println(r)

//   val rr = list1.histo(odd)
//   println(rr)

//   // val rrr = list1.apo[Fix[IntList]](mapHead(_ + 1))

//   val l = List(3, 1, 123 ,45, 222 ,12,  45, 13).ana[Fix[IntList]](build)
//   println(l)
//   val rrr = l.cata(insersionSort)
//   // val rrr = l.apo[Fix[IntList]](knockback)
//   println(rrr)

//   // val result4 = postpro(1)(small, infiniteStream)
//   // println(result4)
// }
