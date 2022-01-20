package cherry.utils

import cats.Monad
import cats.StackSafeMonad
import cats.Parallel
import cats.arrow.FunctionK
import cats.Applicative

enum Act[State, +Err, +Res]:
  case State(f: State => State)
  case Error(e: Err)
  case Result(r: Res)

  case FlatMap[State, +Err, Mid, +Res](
      base: Act[State, Err, Mid],
      cont: Mid => Act[State, Err, Res]
  ) extends Act[State, Err, Res]

  case Par[State, +Err, X, Y, +Res](
      left: Act[State, Err, X],
      right: Act[State, Err, Y],
      f: (X, Y) => Res
  ) extends Act[State, Err, Res]

  def flatMap[R, E >: Err](f: Res => Act[State, E, R]): Act[State, E, R]                 = FlatMap(this, f)
  def map[R](f: Res => R): Act[State, Err, R]                                            = flatMap(x => Result(f(x)))
  def map2Par[E >: Err, B, C](act: Act[State, E, B])(f: (Res, B) => C): Act[State, E, C] = Par(this, act, f)

  def run(init: State): (State, Either[Vector[Err], Res]) = ???

object Act:
  given [S, E]: StackSafeMonad[[R] =>> Act[S, E, R]] with
    def pure[A](a: A)                                         = Act.Result(a)
    def flatMap[A, B](fa: Act[S, E, A])(f: A => Act[S, E, B]) = fa.flatMap(f)
  end given

  given [S, E]: Parallel[[R] =>> Act[S, E, R]] with FunctionK[[R] =>> Act[S, E, R], [R] =>> Act[S, E, R]] with
    type F[A] = Act[S, E, A]
    def apply[A](a: Act[S, E, A]) = a
    def parallel                  = this
    def sequential                = this
    val monad                     = summon

    given applicative: Applicative[F] with
      def pure[A](a: A): Act[S, E, A] = Act.Result(a)
      def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = map2(ff, fa)(_(_))
      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = fa.map2Par(fb)(f)
end Act
