package cherry.fix

trait Defer[F[_]] extends Functor[F]:
  extension [A](fa: => F[A]) def defer: F[A]
