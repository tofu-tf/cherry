package cherry.utils

import cherry.fix.Monad
import cherry.utils.Act.Local

import scala.annotation.{tailrec, targetName}
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

enum Act[-State, +Res]:
  case Local[-S1, S2, +Res](act: Act[S2, Res], f: S1 => S2) extends Act[S1, Res]

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

  def flatMap2Par[S <: State, B, C](act: Act[S, B])(f: (Res, B) => Act[S, C]): Act[S, C] = Par(this, act, f)

  def local[S1](f: S1 => State): Act[S1, Res] = Act.Local(this, f)

  def localWith[S](f: Act[S, State]): Act[S, Res] = f.flatMap(s => local(_ => s))

  def provide(s: State): Act[Any, Res] = local(_ => s)

  @targetName("followedBy")
  infix def *>[S <: State, B](act: => Act[S, B]): Act[S, B] = flatMap(_ => act)

  private def runIter(state: State, maxSteps: Long = -1): TailRec[Null | Res] =
    if maxSteps == 0 then throw IllegalStateException("maximum steps exhausted")
    else
      this match
        case Action(f)           => TailCalls.done(f(state))
        case Local(act, f)       => act.runIter(f(state), maxSteps - 1)
        case Par(l, Act.unit, f) => // quick way for flatMap
          l.runIter(state).flatMap(res => if res != null then f(res, ()).runIter(state, maxSteps - 1) else Act.stop)
        case Par(l, r, f)        =>
          l.runIter(state, maxSteps - 1).flatMap { lres =>
            r.runIter(state, maxSteps - 1).flatMap { rres =>
              if lres != null && rres != null then f(lres, rres).runIter(state, maxSteps - 1) else Act.stop
            }
          }

  def run(init: State, maxSteps: Long = -1): Option[Res] =
    val res = runIter(init, maxSteps).result
    if res == null then None else Some(res)
end Act

trait Raising[S, E]:
  extension (s: S) def raise(err: => E): Any

object Act:
  private val stop: TailRec[Null] = TailCalls.done(null)

  val none: Act[Any, Nothing] = action(_ => null)
  val unit: Act[Any, Unit]    = pure(())

  def pure[A](a: A): Act[Any, A] = Act.Action(_ => a)

  def defer[S, A](fa: => Act[S, A]) = unit.flatMap(_ => fa)

  def get[S]: Act[S, S] = Action(x => x)

  def read[S, A](f: S => A): Act[S, A] = Action(f)

  def action[S, A](f: S => A | Null): Act[S, A] = Action(f)

  def error[S, E](e: => E)(using Raising[S, E]): Act[S, Nothing] = Act.Action { s =>
    s.raise(e)
    null
  }

  def option[S, E, A](oa: Option[A], err: => E)(using ctx: Raising[S, E]): Act[S, A] =
    oa match
      case None    => error(err)
      case Some(a) => Act.pure(a)

  def optionF[S, A](oa: Option[A], default: => Act[S, A]): Act[S, A] =
    oa match
      case None    => default
      case Some(a) => pure(a)

  def either[S, E, A](ea: Either[E, A])(using ctx: Raising[S, E]): Act[S, A] =
    ea match
      case Left(e)  => error(e)
      case Right(a) => pure(a)

  given [S]: Monad[[R] =>> Act[S, R]] with
    def pure[A](a: A)                                              = Act.pure(a)
    extension [A](fa: Act[S, A]) def flatMap[B](f: A => Act[S, B]) = fa.flatMap(f)

  extension [S, A](state: Act[S, A]) def locally(f: S => S): Act[S, A] = Act.Local(state, f)
end Act

trait ActMethods[S]:
  val none: Act[Any, Nothing] = Act.none
  val unit: Act[Any, Unit]    = Act.unit

  def pure[A](a: A): Act[Any, A] = Act.Action(_ => a)

  def defer[A](fa: => Act[S, A]) = Act.unit.flatMap(_ => fa)

  val get: Act[S, S] = Act.Action(x => x)

  def read[A](f: S => A): Act[S, A] = Act.Action(f)

  def action[A](f: S => A | Null): Act[S, A] = Act.Action(f)

  def error[E](e: => E)(using Raising[S, E]): Act[S, Nothing] = Act.Action { s =>
    s.raise(e)
    null
  }

  def option[E, A](oa: Option[A], err: => E)(using Raising[S, E]): Act[S, A] =
    oa match
      case None    => error(err)
      case Some(a) => Act.pure(a)

  def optionF[A](oa: Option[A], default: => Act[S, A]): Act[S, A] =
    oa match
      case None    => default
      case Some(a) => pure(a)

  def either[E, A](ea: Either[E, A])(using Raising[S, E]): Act[S, A] =
    ea match
      case Left(e)  => error(e)
      case Right(a) => pure(a)
