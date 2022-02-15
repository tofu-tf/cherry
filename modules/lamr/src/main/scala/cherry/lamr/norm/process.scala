package cherry.lamr.norm

import tofu.data.{CalcT, CalcM}
import cats.Parallel
import cats.arrow.FunctionK
import cats.Applicative
import cats.syntax.applicative
import cherry.utils.Act
import cherry.lamr.RecordKey
import cherry.lamr.LibRef

case class State(
    var symbolCount: Long = 0,
    var inequasions: InequasionSystem[PartialTerm] = DummyIneqSystem(),
    var symbols: Map[Long, RecordKey] = Map.empty,
    var position: Option[Position] = None,
    var term: Option[PartialTerm] = None,
    var errors: Vector[Error] = Vector.empty,
) extends Act.Raising[Cause]:
  def error(e: => Cause) = errors :+= Error(e, term, position)

case class Position(start: Long, end: Long):
  def set: Process[Unit] = Act.Action(_.position = Some(this))

enum TypeCause:
  case Record, Function, Type

enum Cause:
  case MissingLibrary(name: String)
  case MissingRef(ref: LibRef)
  case BadType(expected: TypeCause)
  case MissingKey(key: RecordKey)
  case UnrelatedValue

  case Abort(message: String)

  def raise: Process[Nothing] = Act.error(this)

case class Error(
    cause: Cause,
    term: Option[PartialTerm] = None,
    position: Option[Position] = None,
)

end Error

type Process[+A] = Act[State, A]

def newSymbol[R](name: String, tpe: R): Process[Symbol[R]] =
  Act.action { state =>
    state.symbolCount += 1
    val id = state.symbolCount
    state.symbols += id -> name
    Symbol(id, name, tpe)
  }
