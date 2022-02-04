package cherry.lamr.norm

import tofu.data.{CalcT, CalcM}
import cats.Parallel
import cats.arrow.FunctionK
import cats.Applicative
import cats.syntax.applicative
import cherry.utils.Act
import cherry.lamr.RecordKey

case class State(
    var symbolCount: Long,
    var inequasions: InequasionSystem[PartialTerm],
    var position: Option[Position],
    var term: Option[PartialTerm],
    var errors: Vector[Error],
) extends Act.Raising[Error]:
  def error(e: => Error) = errors :+= e

case class Position(start: Long, end: Long):
  def set: Process[Unit] = Act.Action(_.position = Some(this))

enum TypeCause:
  case Record, Function, Type


enum Cause:
  case MissingLibrary(name: String)
  case BadType(expected: TypeCause)
  case MissingKey(key: RecordKey)

  case Abort(message: String)

case class Error(
    cause: Cause,
    term: Option[PartialTerm] = None,
    position: Option[Position] = None,
):

  def raise: Process[Nothing] = Act.error(this)

type Process[+A] = Act[State, A]

def newSymbol(name: String): Process[Symbol] =
  Act.action { state =>
    state.symbolCount += 1
    val symbol = Symbol(state.symbolCount, name)
    symbol
  }
