package recursion
package expr

import cats._, cats.implicits._

object basic {
  sealed trait Expr
  case class Lit(value: Int) extends Expr
  case class Add(x: Expr, y: Expr) extends Expr
  case class Multiply(x: Expr, y: Expr) extends Expr
}

sealed trait Expr[A]
case class Lit[A](value: Int) extends Expr[A]
case class Add[A](x: A, y: A) extends Expr[A]
case class Multiply[A](x: A, y: A) extends Expr[A]

object Expr {
  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    def map[A, B](expr: Expr[A])(f: A => B): Expr[B] = expr match {
      case Lit(v) => Lit(v)
      case Add(x, y) => Add(f(x), f(y))
      case Multiply(x, y) => Multiply(f(x), f(y))
    }
  }
}

object Playground extends App {

  val evaluate: Expr[Int] => Int = {
    case Lit(v) => v
    case Add(x, y) => x + y
    case Multiply(x, y) => x * y
  }

  val show: Expr[String] => String = {
    case Lit(v) => s"$v"
    case Add(x, y) => s"($x + $y)"
    case Multiply(x, y) => s"($x * $y)"
  }

  def lit(value: Int): Fix[Expr] = Fix(Lit(value))
  def multiply(x: Fix[Expr], y: Fix[Expr]): Fix[Expr] = Fix(Multiply(x, y))
  def add(x: Fix[Expr], y: Fix[Expr]): Fix[Expr] = Fix(Add(x, y))

  val expr: Fix[Expr] = add(lit(5), multiply(lit(2), lit(4)))

  val result = cata(expr)(evaluate)
  println(result)

  val string = cata(expr)(show)
  println(string)
}
