package cherry.lamr.parse

import cherry.fix.{Monad, Semigroupal}
import cats.parse.*

given Semigroupal[Parser] with

  extension [A](fa: Parser[A])
    def map[B](f: A => B): Parser[B]              = fa.map(f)
    def map2[B, C](fb: Parser[B])(f: (A, B) => C) =
      Parser.product10(fa, fb).map((a, b) => f(a, b))

given Monad[Parser0] with
  def pure[A](a: A): Parser0[A] = Parser.pure(a)

  extension [A](fa: Parser0[A])
    def flatMap[B](f: A => Parser0[B]): Parser0[B] = fa.flatMap(f)
    override def map[B](f: A => B): Parser0[B]     = fa.map(f)
