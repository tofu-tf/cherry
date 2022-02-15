package cherry.lamr.norm
package umami

import cherry.fix.Fix
import cherry.lamr.{BuiltinType, Lang, RecordKey, TypeOptions}
import cherry.utils.Act

import scala.collection.immutable.TreeMap
import scala.collection.immutable.IntMap
import cherry.utils.LayeredMap
import cherry.lamr.norm.umami.RecordValue.fromVector

case class Abstract(term: PartialTerm, tpe: NormType) extends NormValue:
  def toPartial = term

  override def isAbstract = true

  private def make(term: PartialTerm, tpe: Process[NormType]): Process[NormValue] =
    tpe.map(Abstract(term, _))

  override def apply(arg: NormValue)                                              =
    make(term |> arg.toPartial, tpe.applied(arg))

  override def get(key: RecordKey, up: Int)                                       =
    make(term |> Lang.GetKey(key, up), tpe.got(key, up))

end Abstract

case class RecordValue(map: LayeredMap[RecordKey, NormValue]) extends NormValue:
  def toPartial = joinAll(map.journal.iterator.map(toRecord))

  private def toRecord(key: RecordKey, value: NormValue): PartialTerm =
    Lang.set(key, value.toPartial)

  private def joinAll(it: IterableOnce[PartialTerm]): PartialTerm     =
    it.iterator.foldLeft[PartialTerm](Lang.Unit)((rec, set) => Lang.Merge(rec, set).fix)

  override def merge(term: NormValue): Process[NormValue]             = term match
    case RecordValue(extMap) => Act.pure(fromVector(map.journal ++ extMap.journal))
    case _                   => super.merge(term)

  override def get(key: RecordKey, up: Int): Process[NormValue]       =
    Act.option(map.get(key, up), Cause.MissingKey(key))
end RecordValue

object RecordValue:
  def single(key: RecordKey, v: NormValue) = fromVector(Vector(key -> v))

  def fromVector(kvs: Vector[(RecordKey, NormValue)]) = RecordValue(LayeredMap.fromVector(kvs))

  def from(kvs: (RecordKey, NormValue)*) = fromVector(kvs.toVector)

case class Closure(context: NormValue, body: PartialTerm, domain: NormType, norm: Normalizer) extends NormValue:
  def toPartial                      =
    val func = Lang.Capture(domain.toPartial, body).fix
    if context.isUnit then func else context.toPartial |> func

  override def apply(arg: NormValue) =
    for
      narrowArg   <- arg.narrow(domain)
      fullContext <- context.merge(narrowArg)
      res         <- norm.normalize(body, context)
    yield res
end Closure

case class Merge(base: NormValue, ext: NormValue)                                             extends NormValue:
  def toPartial = Lang.Extend(base.toPartial, ext.toPartial).fix

  override def merge(ext2: NormValue) = ext.merge(ext2).flatMap(base.merge)

case class Narrow(base: NormValue, expect: NormType)                                          extends NormValue:
  def toPartial = Lang.Narrow(base.toPartial, expect.toPartial).fix

case object UnitValue                                                                         extends NormValue:
  def toPartial = Lang.Unit

  val pure = Act.pure(this)

  override def isUnit: Boolean = true

  override def narrow(domain: NormType): Process[NormValue] = pure

  override def merge(term: NormValue): Process[NormValue] = Act.pure(term)

trait BuiltinTypeValue(bt: BuiltinType) extends NormValue:
  override def narrow(domain: NormType): Process[NormValue] =
    domain match
      case BuiltinNormType(`bt`, _) => Act.pure(this)
      case _                        => super.narrow(domain)

case class IntegerValue(value: BigInt)  extends BuiltinTypeValue(BuiltinType.Integer):
  def toPartial = Lang.Integer(value)

case class StringValue(value: String)   extends BuiltinTypeValue(BuiltinType.Str):
  def toPartial = Lang.Str(value)

case class FloatValue(value: Double)    extends BuiltinTypeValue(BuiltinType.Float):

  def toPartial = Lang.Float(value)

case class BooleanValue(value: Boolean) extends BuiltinTypeValue(BuiltinType.Bool):

  def toPartial = Lang.Bool(value)
