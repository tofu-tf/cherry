package cherry.fix

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

final case class Cofree[+F[+_], +R](head: TailRec[R], tail: TailRec[F[Cofree[F, R]]])

object Cofree:
  given [F[+_]: Functor]: Comonad[[A] =>> Cofree[F, A]] with
    extension [A](cf: Cofree[F, A])
      def extract: A                                    = cf.head.result
      override def map[B](f: A => B): Cofree[F, B]      =
        Cofree(
          cf.head.map(f),
          cf.tail.map(_.map(_.map(f)))
        )
      def cobind[B](f: Cofree[F, A] => B): Cofree[F, B] =
        Cofree(
          delay(f(cf)),
          cf.tail.map(_.map(_.cobind(f)))
        )
