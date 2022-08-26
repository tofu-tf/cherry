package cherry.fix

trait Monoidal[F[_]] extends Semigroupal[F]:
  def pure[A](a: A): F[A]

  extension [A](fa: F[A]) def map[B](f: A => B): F[B] = fa.map2(unit)((a, _) => f(a))

  val unit        = pure(())
  val emptyTuple  = pure(EmptyTuple)
  val none        = pure(None)
  val emptyVector = pure(Vector.empty)

  extension [A, B](fab: F[A => B]) def ap(fa: F[A]): F[B]                     = fab.map2(fa)(_(_))
  extension [A, B, C](fabc: F[(A, B) => C]) def ap2(fa: F[A], fb: F[B]): F[C] = (fabc, fa, fb).mapN(_(_, _))

  def collectVector[A, B](v: Vector[A])(f: A => F[Option[B]]): F[Vector[B]] =
    if v.isEmpty then emptyVector.widen
    else if v.size == 1 then f(v(0)).map(_.toVector)
    else
      val (left, right) = v.splitAt(v.size / 2)
      collectVector(left)(f).map2(collectVector(right)(f))(_ ++ _)
