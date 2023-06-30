package cherry.fix

trait Comonad[F[_]] extends Functor[F]:
  extension [A](fa: F[A]) def extract: A
  def cobind[B](f: F[A] => B): F[B]

  override def map[B](f: A => B): F[B] = cobind(fa => f(fa.extract))
