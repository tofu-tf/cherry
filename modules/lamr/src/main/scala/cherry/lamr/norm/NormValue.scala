package cherry.lamr.norm

import cherry.lamr.{BuiltinType, Lang, LibRef, RecordKey}
import cherry.fix.Fix
import tofu.syntax.*
import cats.syntax.show.*
import cherry.utils.Act
import cherry.lamr.norm.umami.{BuiltinNormType, IntegerValue, Narrow, NormType, UnitValue}

case class Symbol[+T](id: Long, key: RecordKey, tpe: T)

type Partial[+A] = Lang[A] | Symbol[A]

type PartialTerm = Fix[Partial]
trait Normalizer:
  def normalize(term: PartialTerm, context: NormValue): Process[NormValue]

trait NormValue:
  def toPartial: PartialTerm

  def headNorm: Process[NormValue] = Act.pure(this)

  def errorDisplay: Option[String] = Some(toPartial.toString)

  def position: Option[Position] = None

  def isAbstract: Boolean = false

  def apply(term: NormValue): Process[NormValue] = Cause.BadType(TypeCause.Function).raise

  def get(key: RecordKey, up: Int): Process[NormValue] = Cause.BadType(TypeCause.Record).raise

  def asType: Process[NormType] = Cause.BadType(TypeCause.Type).raise

  def merge(term: NormValue): Process[NormValue] = Act.pure(umami.Merge(this, term))

  def narrow(domain: NormType): Process[NormValue] =
    if domain == BuiltinNormType(BuiltinType.Any) then UnitValue.pure
    else Act.error(Cause.UnrelatedValue)

  def first = get(0, 0)

  def second = get(1, 0)

  def isUnit: Boolean = false

  def asInt: Process[BigInt] = this match
    case IntegerValue(v) => Act.pure(v)
    case _               => Act.error(Cause.BadType(TypeCause.Builtin(BuiltinType.Integer)))
