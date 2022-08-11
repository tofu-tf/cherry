package cherry.fix

trait Traverse[T[_]] extends Functor[T]:
  extension [A](ta: T[A])
    def traverse[F[+_]: Monoidal, B](f: A => F[B]): F[T[B]]
    def parTraverse[F[+_]: Monoidal, B](f: A => F[B]): F[T[B]] = traverse(f)
    override def map[B](f: A => B): T[B] = ta.traverse[[x] =>> x, B](f)

  extension [A, F[+_]](ta: T[F[A]]) def sequence(using Monoidal[F]): F[T[A]] = ta.traverse(x => x)

object Traverse:
  inline def derived[T[+a]]: Traverse[T] = derivation.traverse.deriveTraverse
