package cherry.fix

import cats.Eval
import cats.Comonad
import tofu.syntax.monadic.given
import cats.Functor

final case class Cofree[+F[+_], +R](head: Eval[R], tail: Eval[F[Cofree[F, R]]])

object Cofree:
  given [F[+_]: Functor]: Comonad[[A] =>> Cofree[F, A]] with
    def extract[A](cf: Cofree[F, A]): A                                       = cf.head.value
    def map[A, B](fa: Cofree[F, A])(f: A => B): Cofree[F, B]                  =
      Cofree(
        fa.head.map(f),
        fa.tail.map(_.map(map(_)(f)))
      )
    def coflatMap[A, B](fa: Cofree[F, A])(f: Cofree[F, A] => B): Cofree[F, B] =
      Cofree(
        Eval.later(f(fa)),
        fa.tail.map(_.map(coflatMap(_)(f)))
      )
