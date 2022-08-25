package cherry.fix

trait Semigroupal[F[_]] extends Functor[F]:
  extension [A](fa: F[A])
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C]
    def zip[B](fb: F[B]): F[(A, B)] = fa.map2(fb)((_, _))
    def >>[B](fb: F[B]): F[B]       = map2(fb)((_, y) => y)

  extension [A, T <: Tuple](fa: F[A] *: T)
    def tupled(using u: Untuple[F, F[A] *: T]): F[A *: Untupled[F, T]]               = 
      u.zipAll(fa)(using this)
    def mapN[C](using u: Untuple[F, F[A] *: T])(f: (A *: Untupled[F, T]) => C): F[C] = 
      u.zipAll(fa)(using this).map(f)
