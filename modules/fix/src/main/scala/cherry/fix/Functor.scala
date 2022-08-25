package cherry.fix

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]
    def widen[A1 >: A]: F[A1] = fa.asInstanceOf[F[A1]]

  extension [G[_], A](fa: F[G[A]])
    def mapIn[B](f: A => B)(using Functor[G]): F[G[B]] =
      fa.map(_.map(f))

object Functor:
  def pure[F[_]](using F: Monoidal[F]) = [A] => (x: A) => F.pure(x)
  def none[F[_]](using F: Monoidal[F]) = F.none
  def unit[F[_]](using F: Monoidal[F]) = F.unit

  given Monad[[x] =>> x] with Traverse[[x] =>> x] with
    def pure[A](a: A) = a
    extension [A](a: A)
      def flatMap[B](f: A => B): B                         = f(a)
      def traverse[F[+_]: Monoidal, B](f: A => F[B]): F[B] = f(a)

  given Monad[TailRec] with Defer[TailRec] with
    def pure[A](a: A)                                                = TailCalls.done(a)
    extension [A](fa: TailRec[A]) def flatMap[B](f: A => TailRec[B]) = fa.flatMap(f)

    extension [A](fa: => TailRec[A]) def defer: TailRec[A] = TailCalls.tailcall(fa)

  given [Y: Monoid]: Monad[[x] =>> (Y, x)] with
    def pure[A](a: A)                = (Monoid.zero, a)
    extension [A](a: (Y, A))
      def flatMap[B](f: A => (Y, B)) =
        val (ys, b) = f(a._2)
        (a._1 ++ ys, b)

  given [D]: Monad[[x] =>> D => x] with
    def pure[A](a: A) = _ => a

    extension [A](a: D => A) def flatMap[B](f: A => D => B): D => B = d => f(a(d))(d)

  given TraverseFilter[Option] with
    extension [A](fa: Option[A])
      def traverseFilter[F[+_]: Monoidal, B](f: A => F[Option[B]]): F[Option[B]] = fa match
        case Some(a) => f(a)
        case None    => Functor.none[F]

  given TraverseFilter[Vector] with
    extension [A](fa: Vector[A])
      def traverseFilter[F[+_], B](f: A => F[Option[B]])(using F: Monoidal[F]): F[Vector[B]] =
        F.collectVector(fa)(f)
