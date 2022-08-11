package cherry.fix

trait TraverseFilter[T[_]]:
  extension [A](fa: T[A])
    def traverseFilter[F[+_]: Monoidal, B](b: A => F[Option[B]]): F[T[B]]
    def traverse[F[+_]: Monoidal, B](f: A => F[B]): F[T[B]] = traverseFilter(a => f(a).map(Some(_)))
