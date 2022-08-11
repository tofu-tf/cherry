package cherry.fix

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

trait Functor[F[_]]:
  def fmap[A, B](fa: F[A])(f: A => B): F[B]

  extension [A](fa: F[A]) def map[B](f: A => B): F[B] = fmap(fa)(f)

trait Monoidal[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  def zipMap[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  val unit       = pure(())
  val emptyTuple = pure(EmptyTuple)

  def fmap[A, B](fa: F[A])(f: A => B): F[B] = zipMap(fa, unit)((a, _) => f(a))

  extension [A](fa: F[A])
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = zipMap(fa, fb)(f)
    def zip[B](fb: F[B]): F[(A, B)]                = zipMap(fa, fb)((_, _))

  extension [A, B](fab: F[A => B]) def ap(fa: F[A]): F[B]                     = fab.map2(fa)(_(_))
  extension [A, B, C](fabc: F[(A, B) => C]) def ap2(fa: F[A], fb: F[B]): F[C] = (fabc, fa, fb).mapN(_(_, _))

  extension [A, T <: Tuple](fa: F[A] *: T)
    def tupled(using u: Untuple[F, F[A] *: T]): F[A *: Untupled[F, T]]               = u.zipAll(fa)(using this)
    def mapN[C](using u: Untuple[F, F[A] *: T])(f: (A *: Untupled[F, T]) => C): F[C] = u.zipAll(fa)(using this).map(f)

trait Monad[F[_]] extends Monoidal[F]:
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def zipMap[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    bind(fa)(a => bind(fb)(b => pure(f(a, b))))

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]                    = bind(fa)(f)
    def flatMap2[B, C](fb: F[B])(f: (A, B) => F[C]): F[C] = bind(fa)(a => bind(fb)(b => f(a, b)))

object Monad:
  def pure[F[_]](using F: Monoidal[F]) = [A] => (x: A) => F.pure(x)

trait Traverse[T[_]] extends Functor[T]:
  def trav[A, B, F[+_]: Monoidal](ta: T[A])(f: A => F[B]): F[T[B]]

  override def fmap[A, B](fa: T[A])(f: A => B): T[B] = trav[A, B, [x] =>> x](fa)(f)

  extension [A](ta: T[A])
    def traverse[F[+_]: Monoidal, B](f: A => F[B]): F[T[B]]         = trav(ta)(f)
    def sequence[F[+_]: Monoidal, B](using ev: A <:< F[B]): F[T[B]] = trav(ta)(ev)

object Traverse:
  inline def derived[T[+a]]: Traverse[T] = derivation.traverse.deriveTraverse

object Functor:
  given Monad[[x] =>> x] with Traverse[[x] =>> x] with
    def pure[A](a: A)                                         = a
    def bind[A, B](a: A)(f: A => B): B                        = f(a)
    def trav[A, B, F[+_]: Monoidal](a: A)(f: A => F[B]): F[B] = f(a)

  given Monad[TailRec] with
    def pure[A](a: A)                                  = TailCalls.done(a)
    def bind[A, B](fa: TailRec[A])(f: A => TailRec[B]) = fa.flatMap(f)

  given [Y: Monoid]: Monad[[x] =>> (Y, x)] with
    def pure[A](a: A)                         = (Monoid.zero, a)
    def bind[A, B](a: (Y, A))(f: A => (Y, B)) =
      val (ys, b) = f(a._2)
      (a._1 ++ ys, b)

  given [D]: Monad[[x] =>> D => x] with
    def pure[A](a: A)                                 = _ => a
    def bind[A, B](a: D => A)(f: A => D => B): D => B = d => f(a(d))(d)

type Untupled[F[_], T <: Tuple] <: Tuple = T match
  case F[a] *: t  => a *: Untupled[F, t]
  case EmptyTuple => EmptyTuple

trait Untuple[F[_], T <: Tuple]:
  def zipAll(ft: T)(using Monoidal[F]): F[Untupled[F, T]]

object Untuple:
  given [F[_]]: Untuple[F, EmptyTuple] with
    def zipAll(_et: EmptyTuple)(using F: Monoidal[F]) = F.emptyTuple

  given [F[_], H, T <: Tuple](using T: Untuple[F, T]): Untuple[F, F[H] *: T] with
    def zipAll(ft: F[H] *: T)(using Monoidal[F]) = ft.head.map2(T.zipAll(ft.tail))(_ *: _)
