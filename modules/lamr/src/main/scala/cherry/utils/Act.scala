package cherry.utils

import scala.annotation.tailrec
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

import cats.arrow.FunctionK
import cats.syntax.parallel.given
import cats.{Applicative, Eval, Monad, Parallel, StackSafeMonad}

enum Act[State, +Err, +Res]:
  case Pure(res: Res)
  case Error(err: Err)
  case Get[State, +Res](what: State => Res) extends Act[State, Nothing, Res]
  case Set[State](s: State) extends Act[State, Nothing, Unit]
  case FlatMap[State, Mid, +Err, +Res](
      base: Act[State, Err, Mid],
      f: Mid => Act[State, Err, Res]
  ) extends Act[State, Err, Res]

  case Par[State, +Err, X, Y, +Res](
      left: Act[State, Err, X],
      right: Act[State, Err, Y],
      combine: (X, Y) => Res,
  ) extends Act[State, Err, Res]

  def flatMap[R, E >: Err](f: Res => Act[State, E, R]): Act[State, E, R]                 = FlatMap(this, f)
  def map[R](f: Res => R): Act[State, Err, R]                                            = flatMap(x => Act.pure(f(x)))
  def map2Par[E >: Err, B, C](act: Act[State, E, B])(f: (Res, B) => C): Act[State, E, C] =
    Par(this, act, f)

  private def runIter(init: State): TailRec[(State, Either[Vector[Err], Res])]           = this match
    case Pure(x)          => TailCalls.done((init, Right(x)))
    case Error(err)       => TailCalls.done((init, Left(Vector(err))))
    case Get(f)           => TailCalls.done((init, Right(f(init))))
    case Set(st)          => TailCalls.done((st, Right(())))
    case FlatMap(base, f) =>
      base.runIter(init).flatMap {
        case (st, Left(errs)) => TailCalls.done((st, Left(errs)))
        case (st, Right(x))   => f(x).runIter(st)
      }
    case Par(l, r, f)     =>
      l.runIter(init).flatMap { (mid, lres) =>
        r.runIter(mid).map { (fin, rres) =>
          (fin, (lres, rres).parMapN(f))
        }
      }

  def run(init: State): (State, Either[Vector[Err], Res]) = runIter(init).result
end Act
object Act:
  def pure[S, A](a: A): Act[S, Nothing, A] = Act.Pure(a)

  def get[S]: Act[S, Nothing, S] = Get(x => x)

  def state[S, A](f: S => (S, A)): Act[S, Nothing, A] = for {
    start    <- get[S]
    (next, a) = f(start)
    _        <- Set(next)
  } yield a

  def option[S, E, A](oa: Option[A], err: => E): Act[S, E, A] =
    oa match
      case None    => Act.Error(err)
      case Some(a) => Act.Pure(a)

  given [S, E]: StackSafeMonad[[R] =>> Act[S, E, R]] with
    def pure[A](a: A)                                         = Act.pure(a)
    def flatMap[A, B](fa: Act[S, E, A])(f: A => Act[S, E, B]) = fa.flatMap(f)

  given [S, E]: Parallel[[R] =>> Act[S, E, R]] with FunctionK[[R] =>> Act[S, E, R], [R] =>> Act[S, E, R]] with
    type F[A] = Act[S, E, A]
    def apply[A](a: Act[S, E, A]) = a
    def parallel                  = this
    def sequential                = this
    val monad                     = summon

    val applicative: Applicative[F] = new:
      def pure[A](a: A): Act[S, E, A]                                = Act.pure(a)
      def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]                    = map2(ff, fa)(_(_))
      override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) = fa.map2Par(fb)(f)
end Act
