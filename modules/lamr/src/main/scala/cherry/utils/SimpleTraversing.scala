package cherry.utils

import cats.kernel.Monoid
import cats.{Applicative, Eval, Traverse}

trait SimpleTraversing[F[+_], +A]:
  self: F[A] =>
  def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]]

  def map[B](f: A => B): F[B] = traverse[[x] =>> x, B](f)

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    traverse[[a] =>> ConstEndo[B, a, true], B](a => f(_, a)).apply(b)

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    traverse[[a] =>> ConstEndo[Eval[B], a, false], B](a => f(a, _)).apply(lb)

object SimpleTraversing:
  def traverseInstance[F[+x] <: SimpleTraversing[F, x]]: Traverse[F] = new:
    def traverse[G[_]: Applicative, A, B](qa: F[A])(f: A => G[B]): G[F[B]]          = qa.traverse(f)
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B                           = fa.foldLeft(b)(f)
    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)

trait ConstEndo[A, +B, b <: Boolean]:
  def apply(a: A): A

object ConstEndo:
  given forwardApplicative[A]: Applicative[[a] =>> ConstEndo[A, a, true]] with
    type T[X] = ConstEndo[A, X, true]
    def pure[B](b: B)                                                    = x => x
    def ap[B, C](ff: T[B => C])(fa: T[B]): T[C]                          = x => ff(fa(x))
    override def map2[B, C, D](fb: T[B], fc: T[C])(f: (B, C) => D): T[D] = x => fb(fc(x))

  given backApplicative[A]: Applicative[[a] =>> ConstEndo[A, a, false]] with
    type T[X] = ConstEndo[A, X, false]
    def pure[B](b: B)                                                    = x => x
    def ap[B, C](ff: T[B => C])(fa: T[B]): T[C]                          = x => fa(ff(x))
    override def map2[B, C, D](fb: T[B], fc: T[C])(f: (B, C) => D): T[D] = x => fc(fb(x))
