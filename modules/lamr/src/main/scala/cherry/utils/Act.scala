package cherry.utils

import scala.annotation.tailrec
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

import cats.arrow.FunctionK
import cats.syntax.parallel.given
import cats.{Applicative, Eval, Monad, Parallel, StackSafeMonad}

enum Act[-State, +Res]:
  case Action(what: State => Res | Null)

  case Par[-State, X, Y, +Res](
      left: Act[State, X],
      right: Act[State, Y],
      combine: (X, Y) => Act[State, Res],
  ) extends Act[State, Res]

  def flatMap[S <: State, R](f: Res => Act[S, R]): Act[S, R]                 = Par(this, Act.unit, (x, _) => f(x))
  def map[R](f: Res => R): Act[State, R]                                     = Par(this, Act.unit, (x, _) => Act.pure(f(x)))
  def map2Par[S <: State, B, C](act: Act[S, B])(f: (Res, B) => C): Act[S, C] =
    Par(this, act, (x, y) => Act.pure(f(x, y)))

  private def runIter(state: State): TailRec[Null | Res]                     = this match
    case Action(f)    => TailCalls.done(f(state))
    case Par(l, r, f) =>
      l.runIter(state).flatMap { lres =>
        r.runIter(state).flatMap { rres =>
          if lres != null && rres != null then f(lres, rres).runIter(state) else Act.stop
        }
      }

  def run(init: State): Option[Res] =
    val res = runIter(init).result
    if res == null then None else Some(res)
end Act

object Act:
  private val stop: TailRec[Null] = TailCalls.done(null)

  val none: Act[Any, Nothing] = action(_ => null)
  val unit: Act[Any, Unit]    = pure(())

  def pure[A](a: A): Act[Any, A] = Act.Action(_ => a)

  def get[S]: Act[S, S] = Action(x => x)

  def action[S, A](f: S => A | Null): Act[S, A] = Action(f)

  def error[E](e: => E): Act[Raising[E], Nothing] = Action { s =>
    s.error(e)
    null
  }

  def option[E, A](oa: Option[A], err: => E): Act[Raising[E], A] =
    oa match
      case None    => error(err)
      case Some(a) => pure(a)

  def either[E, A](ea: Either[E, A]): Act[Raising[E], A] =
    ea match
      case Left(e)  => error(e)
      case Right(a) => pure(a)

  given [S, E]: StackSafeMonad[[R] =>> Act[S, R]] with
    def pure[A](a: A)                                   = Act.pure(a)
    def flatMap[A, B](fa: Act[S, A])(f: A => Act[S, B]) = fa.flatMap(f)

  given [S]: Parallel[[R] =>> Act[S, R]] with FunctionK[[R] =>> Act[S, R], [R] =>> Act[S, R]] with
    type F[A] = Act[S, A]
    def apply[A](a: Act[S, A]) = a
    def parallel               = this
    def sequential             = this
    val monad                  = summon

    val applicative: Applicative[F] = new:
      def pure[A](a: A): Act[S, A]                                   = Act.pure(a)
      def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]                    = map2(ff, fa)(_(_))
      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = fa.map2Par(fb)(f)

  trait Raising[E]:
    def error(err: => E): Any
end Act
