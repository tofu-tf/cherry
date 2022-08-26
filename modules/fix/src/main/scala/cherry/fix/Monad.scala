package cherry.fix

trait Monad[F[_]] extends Monoidal[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

    def >>=[B](f: A => F[B]): F[B]                        = fa.flatMap(f)
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C]        = fa.flatMap(a => fb.flatMap(b => pure(f(a, b))))
    def flatMap2[B, C](fb: F[B])(f: (A, B) => F[C]): F[C] = fa.flatMap(a => fb.flatMap(b => f(a, b)))

object Monad:
  export Functor.{none, pure, unit}
