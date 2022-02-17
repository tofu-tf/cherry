package cherry.lamr.norm

import cherry.lamr.{BuiltinType, Lang, LibRef, RecordKey}
import cherry.fix.Fix
import tofu.syntax.*
import cats.syntax.show.*
import cherry.utils.{Act, ErrorCtx}
import cherry.lamr.norm.umami.{BuiltinNormType, IntegerValue, Narrow, NormType, UnitValue}

case class Symbol[+T](id: Long, key: RecordKey, tpe: T)

type Partial[+A] = Lang[A] | Symbol[A]

type PartialTerm = Fix[Partial]
trait Normalizer:
  def normalize(term: PartialTerm, context: NormValue): Process[NormValue]

trait NormValue:
  given ErrorCtx[State] = _.value = Some(this)

  def toPartial: PartialTerm

  def viewPartial(view: NormValue): PartialTerm = toPartial

  def headNorm: Process[NormValue] = Act.pure(this)

  def errorDisplay: Option[String] = Some(toPartial.toString)

  def position: Option[Position] = None

  def isAbstract: Boolean = false

  def apply(term: NormValue): Process[NormValue] = Act.error(Cause.BadType(TypeCause.Function))

  def get(key: RecordKey, up: Int): Process[NormValue] =
    Act.error(Cause.BadType(TypeCause.Record))

  def asType: Process[NormType] = Act.error(Cause.BadType(TypeCause.Type))

  def merge(term: NormValue): Process[NormValue] = Act.pure(umami.Merge(this, term))

  def narrow(domain: NormType): Process[NormValue] =
    if domain == BuiltinNormType(BuiltinType.Any) then UnitValue.pure
    else Act.error(Cause.UnrelatedValue(domain))

  def first = get(0, 0)

  def second = get(1, 0)

  def isUnit: Boolean = false

  def asInt: Process[BigInt] = this match
    case IntegerValue(v) => Act.pure(v)
    case _               => Act.error(Cause.BadType(TypeCause.Builtin(BuiltinType.Integer)))
