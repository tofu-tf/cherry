package cherry.utils

import cherry.fix.{Monoidal, Traverse}

trait SimpleTraversing[F[+_], +A]:
  self: F[A] =>
  def traverse[G[+_]: Monoidal, B](f: A => G[B]): G[F[B]]

object SimpleTraversing:
  def traverseInstance[F[+x] <: SimpleTraversing[F, x]]: Traverse[F] = new:
    extension [A](fa: F[A]) def traverse[G[+_]: Monoidal, B](f: A => G[B]): G[F[B]] = fa.traverse(f)

trait ConstEndo[A, +B, b <: Boolean]:
  def apply(a: A): A

object ConstEndo:
  given forwardApplicative[A]: Monoidal[[a] =>> ConstEndo[A, a, true]] with
    type T[X] = ConstEndo[A, X, true]
    def pure[B](b: B) = x => x

    extension [B](fb: T[B]) override def map2[C, D](fc: T[C])(f: (B, C) => D): T[D] = x => fb(fc(x))

  given backApplicative[A]: Monoidal[[a] =>> ConstEndo[A, a, false]] with
    type T[X] = ConstEndo[A, X, false]
    def pure[B](b: B) = x => x

    extension [B](fb: T[B]) def map2[C, D](fc: T[C])(f: (B, C) => D): T[D] = x => fc(fb(x))
