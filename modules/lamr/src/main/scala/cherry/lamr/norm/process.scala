package cherry.lamr.norm

import tofu.data.{CalcT, CalcM}
import cats.Parallel
import cats.arrow.FunctionK
import cats.Applicative
import cats.syntax.applicative
import cherry.utils.Act




case class State(
    symbolCount: Long,
    inequasions: InequasionSystem[PartialTerm]
)

case class Position(start: Long, end: Long)

enum Cause:
  case MissingLibrary(name: String)
  case BadType(expected: String)

  case Abort(message: String)

case class Error(
    cause: Cause,
    term: Option[PartialTerm] = None,
    position: Option[Position] = None,
):

  def raise: Process[Nothing] = Act.Error(this)

type Process[+A] = Act[State, Error, A]

def newSymbol(name: String): Process[Symbol] =
  Act.state { state =>
    val count  = state.symbolCount + 1
    val symbol = Symbol(count, name)
    (state.copy(symbolCount = count), symbol)
  }
