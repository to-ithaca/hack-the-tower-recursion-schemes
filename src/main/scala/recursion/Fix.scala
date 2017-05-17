package recursion

case class Fix[F[_]](unfix: F[Fix[F]])

sealed trait Free[F[_], A]
case class Pure[F[_], A](a: A) extends Free[F, A]
case class FreeCons[F[_], A](f: F[Free[F, A]]) extends Free[F, A]

case class Cofree[F[_], A](a: A, f: F[Cofree[F, A]])
