package cherry.utils

import cats.Monad
import cats.StackSafeMonad
import cats.Parallel
import cats.arrow.FunctionK
import cats.Applicative
import scala.annotation.tailrec
import cats.Eval
import cats.syntax.parallel.given
import scala.util.control.TailCalls.TailRec
import scala.util.control.TailCalls

enum Act[State, +Err, +Res]:
  case Pure(res: Res)
  case Error(err: Err)
  case Get[State]() extends Act[State, Nothing, State]
  case Set[State](s: State) extends Act[State, Nothing, Unit]
  case FlatMap[State, Mid, +Err, +Res](
      base: Act[State, Err, Mid],
      f: Mid => Act[State, Err, Res]
  ) extends Act[State, Err, Res]

  case Par[State, +Err, X, Y](
      left: Act[State, Err, X],
      right: Act[State, Err, Y],
  ) extends Act[State, Err, (X, Y)]

  def flatMap[R, E >: Err](f: Res => Act[State, E, R]): Act[State, E, R]                 = FlatMap(this, f)
  def map[R](f: Res => R): Act[State, Err, R]                                            = flatMap(x => Act.pure(f(x)))
  def map2Par[E >: Err, B, C](act: Act[State, E, B])(f: (Res, B) => C): Act[State, E, C] =
    Par(this, act).map((x, y) => f(x, y))

  def runIter(init: State): TailRec[(State, Either[Vector[Err], Res])] = this match
    case Pure(x)          => TailCalls.done((init, Right(x)))
    case Error(err)       => TailCalls.done((init, Left(Vector(err))))
    case Get()            => TailCalls.done((init, Right(init)))
    case Set(st)          => TailCalls.done((st, Right(())))
    case FlatMap(base, f) =>
      base.runIter(init).flatMap {
        case (st, Left(errs)) => TailCalls.done((st, Left(errs)))
        case (st, Right(x))   => f(x).runIter(st)
      }
    case Par(l, r)        =>
      l.runIter(init).flatMap { (mid, lres) =>
        r.runIter(mid).map { (fin, rres) =>
          (fin, (lres, rres).parTupled)
        }
      }
object Act:
  def pure[S, A](a: A): Act[S, Nothing, A] = Act.Pure(a)

  enum Next[S, -From, +Err, +Res]:
    /** everything is done, time to complete */
    case Done[S, A]() extends Next[S, A, Nothing, A]

    /** there's still hope, no errors yet */
    case Cont(cont: From => Act[S, Err, Res])

    /** there were some errors in process, we can only start more branches to collect more errors */
    case Errors[S, E](vs: Vector[E], rest: Vector[Act[S, E, Any]]) extends Next[S, Any, E, Nothing]

  private def finita[Err, Res](errs: Vector[Err], res: Res): Either[Vector[Err], Res] =
    if errs.isEmpty then Right(res) else Left(errs)

  given [S, E]: StackSafeMonad[[R] =>> Act[S, E, R]] with
    def pure[A](a: A)                                         = Act.pure(a)
    def flatMap[A, B](fa: Act[S, E, A])(f: A => Act[S, E, B]) = fa.flatMap(f)
  end given

  given [S, E]: Parallel[[R] =>> Act[S, E, R]] with FunctionK[[R] =>> Act[S, E, R], [R] =>> Act[S, E, R]] with
    type F[A] = Act[S, E, A]
    def apply[A](a: Act[S, E, A]) = a
    def parallel                  = this
    def sequential                = this
    val monad                     = summon

    given applicative: Applicative[F] with
      def pure[A](a: A): Act[S, E, A]                                = Act.pure(a)
      def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]                    = map2(ff, fa)(_(_))
      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = fa.map2Par(fb)(f)
end Act
