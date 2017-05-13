package recursion.basic

trait Nat
case object Zero extends Nat
case class Succ(n: Nat) extends Nat

object Recurse {
  
  val number = Succ(Succ(Succ(Zero)))

  def toInt(nat: Nat): Int = nat match {
    case Zero => 0
    case Succ(a) => toInt(a) + 1
  }
}
