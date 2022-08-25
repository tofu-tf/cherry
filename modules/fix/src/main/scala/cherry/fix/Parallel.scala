package cherry.fix

trait Parallel[F[_]] extends Monoidal[F]:
  self =>
  extension [A](fa: F[A]) def parMap2[B, C](fb: F[B])(f: (A, B) => C): F[C]

  extension [A, T <: Tuple](fa: F[A] *: T)
    def parTupled(using u: Untuple[F, F[A] *: T]): F[A *: Untupled[F, T]]               =
      u.zipAll(fa)(using parMonoidal)
    def parMapN[C](using u: Untuple[F, F[A] *: T])(f: (A *: Untupled[F, T]) => C): F[C] =
      u.zipAll(fa)(using parMonoidal).map(f)

  object parMonoidal extends Monoidal[F]:
    def pure[A](a: A): F[A] = self.pure(a)

    extension [A](fa: F[A]) def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = self.parMap2(fa)(fb)(f)
