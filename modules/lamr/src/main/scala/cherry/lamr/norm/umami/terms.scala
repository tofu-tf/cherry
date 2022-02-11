package cherry.lamr.norm
package umami

import cherry.fix.Fix
import cherry.lamr.Lang
import cherry.utils.Act
import cherry.lamr.RecordKey
import scala.collection.immutable.TreeMap
import scala.collection.immutable.IntMap
import cherry.utils.LayeredMap
import cherry.lamr.TypeOptions

case class Abstract(term: PartialTerm, override val position: Option[Position] = None) extends NormValue:
  def toPartial = term

  override def isAbstract = true

  def make(term: PartialTerm): Process[NormValue] = Act.pure(Abstract(term))

  override def apply(arg: NormValue) = make(term >> arg.toPartial)

  override def get(key: RecordKey, up: Int) = make(term >> Lang.Get(key, up))

end Abstract

case class RecordValue(map: LayeredMap[RecordKey, NormValue]) extends NormValue:
  def toPartial = joinAll(map.journal.iterator.map(toRecord))

  private def toRecord(key: RecordKey, value: NormValue): PartialTerm =
    Lang.Set(key, value.toPartial).fix

  private def joinAll(it: IterableOnce[PartialTerm]): PartialTerm     =
    it.foldLeft[PartialTerm](Lang.Unit)((rec, set) => Lang.Merge(rec, set).fix)

  override def get(key: RecordKey, up: Int): Process[NormValue]       =
    Act.option(map.get(key, up), Cause.MissingKey(key))
end RecordValue

object RecordValue:
  def single(key: RecordKey, v: NormValue) = RecordValue(LayeredMap.fromVector(Vector(key -> v)))

case class Closure(context: NormValue, body: PartialTerm, domain: NormType, norm: Normalizer) extends NormValue:
  def toPartial = context.toPartial >> Lang.Capture(domain.toPartial, body).fix

  override def apply(arg: NormValue) =
    for
      narrowArg   <- arg.narrow(domain)
      fullContext <- context.merge(narrowArg)
      res         <- norm.bigStep(body, context)
    yield res
end Closure

case class Merge()

case object UnitValue                                                                         extends NormValue:
  def toPartial = Lang.Unit


case class IntegerValue(value: BigInt)                                                        extends NormValue:
  def toPartial = Lang.Integer(value)

case class StringValue(value: String)                                                         extends NormValue:
  def toPartial = Lang.Str(value)

case class FloatValue(value: Double)                                                          extends NormValue:

  def toPartial = Lang.Float(value)

case class BooleanValue(value: Boolean) extends NormValue:

  def toPartial = Lang.Bool(value)
