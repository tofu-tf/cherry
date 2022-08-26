package cherry.lamr.norm

import cherry.fix.Fix.Fix
import cherry.lamr.norm.umami.{NormType, UnitValue, Variable}
import cherry.utils.{Act, ActMethods, Raising}
import cherry.lamr.{BuiltinType, Lang, LibRef, RecordKey}

case class State(
    var symbolCount: Long = 0,
    var inequasions: InequasionSystem[Term] = DummyIneqSystem(),
    var symbols: Map[Long, RecordKey] = Map.empty,
    var position: Option[Position] = None,
    var errors: Vector[Error] = Vector.empty,
)

case class Position(start: Long, end: Long):
  def set: Process[Unit] = Act.Action(_.state.position = Some(this))

enum TypeCause:
  case Record, Function, Type
  case Builtin(bt: BuiltinType)

enum Cause:
  case MissingLibrary(name: String)
  case MissingRef(ref: LibRef)
  case BadType(expected: TypeCause)
  case MissingKey(key: RecordKey)
  case BadRef(ref: LibRef)
  case UnrelatedValue(t: NormType)

  case Abort(message: String)

  def raise: Process[Nothing] = Act.error(this)

case class Error(
    cause: Cause,
    context: NormValue = UnitValue,
    term: Option[Term] = None,
    position: Option[Position] = None,
)

end Error

type Process[+A] = Act[NormState, A]

object Process extends ActMethods[NormState]:
  val context: Process[NormValue] = Process.read(_.context)

def newSymbol[R](name: String, tpe: R): Process[NormValue] =
  Act.action { ctx =>
    ctx.state.symbolCount += 1
    val id = ctx.state.symbolCount
    ctx.state.symbols += id -> name
    Variable(id, name)
  }
