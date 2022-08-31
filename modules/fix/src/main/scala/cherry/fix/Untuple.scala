package cherry.fix

type Untupled[F[_], T <: Tuple] <: Tuple = T match
  case F[a] *: t  => a *: Untupled[F, t]
  case EmptyTuple => EmptyTuple

trait Untuple[F[_], T <: Tuple]:
  def zipAll(ft: T)(using Semigroupal[F]): F[Untupled[F, T]]

object Untuple:
  given single[A, F[_]]: Untuple[F, F[A] *: EmptyTuple] with
    def zipAll(a: F[A] *: EmptyTuple)(using Semigroupal[F]) = a.head.map(_ *: EmptyTuple)

  given cons[F[_], H, T <: Tuple](using T: Untuple[F, T]): Untuple[F, F[H] *: T] with
    def zipAll(ft: F[H] *: T)(using Semigroupal[F]) = ft.head.map2(T.zipAll(ft.tail))(_ *: _)
