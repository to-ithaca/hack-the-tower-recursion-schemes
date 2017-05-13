import cats._
import cats.implicits._

package object recursion {

  def cata[F[_]: Functor, A](fix: Fix[F])(algebra: F[A] => A): A = {
    algebra(fix.unfix.map(ffix => cata(ffix)(algebra)))
  }

  def ana[F[_]: Functor, A](a: A)(coalgebra: A => F[A]): Fix[F] = {
    Fix(coalgebra(a).map(a => ana(a)(coalgebra)))
  }

  def hylo[F[_] : Functor, A, B](a: A)(algebra: F[B] => B, coalgebra: A => F[A]): B = {
    algebra(coalgebra(a).map(aa => hylo(aa)(algebra, coalgebra)))
  }

  def para[F[_]: Functor, A](fix: Fix[F])(galgebra: F[(Fix[F], A)] => A): A = {
    galgebra(fix.unfix.map { ffix =>
      val a = para(ffix)(galgebra)
      (ffix, a)
    })
  }
}
