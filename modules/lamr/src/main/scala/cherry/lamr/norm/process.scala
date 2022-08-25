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
    var value: Option[NormValue] = None,
    var term: Option[Term] = None,
    var errors: Vector[Error] = Vector.empty,
    val context: NormValue = UnitValue,
)

object State:
  given Raising[State, Cause] with
    extension (s: State)
      def raise(e: => Cause) =
        s.errors :+= Error(e, s.value, s.term, s.position)

case class Position(start: Long, end: Long):
  def set: Process[Unit] = Act.Action(_.position = Some(this))

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
    value: Option[NormValue] = None,
    term: Option[Term] = None,
    position: Option[Position] = None,
)

end Error

type Process[+A] = Act[State, A]

object Process extends ActMethods[State]:
  val context = Process.read(_.context)

def newSymbol[R](name: String, tpe: R): Process[NormValue] =
  Act.action { state =>
    state.symbolCount += 1
    val id = state.symbolCount
    state.symbols += id -> name
    Variable(id, name)
  }
