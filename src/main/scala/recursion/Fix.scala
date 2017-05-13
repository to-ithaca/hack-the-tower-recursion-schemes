package recursion

case class Fix[F[_]](unfix: F[Fix[F]])
