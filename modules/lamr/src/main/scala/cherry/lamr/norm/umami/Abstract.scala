package cherry.lamr.norm
package umami

import cherry.fix.Fix
import cherry.lamr.Lang
import cherry.utils.Act
import cherry.lamr.RecordKey
import scala.collection.immutable.TreeMap
import scala.collection.immutable.IntMap
import cherry.utils.LayeredMap

case class Abstract(term: PartialTerm, val posistion: Option[Position] = None) extends NormValue:
  def toPartial = term

  def apply(arg: PartialTerm) =
    Abstract(Fix(Lang.AndThen(term, arg)), posistion)

case class Record(map: LayeredMap[RecordKey, NormValue])                       extends NormValue:
  def toPartial = joinAll(map.iterator.map(toRecord))

  private def toRecord(it: IterableOnce[(RecordKey, NormValue)]): PartialTerm =
    joinAll(it.iterator.map((k, v) => Lang.Set(k, v.toPartial).fix))

  private def joinAll(it: IterableOnce[PartialTerm]): PartialTerm             =
    it.foldLeft[PartialTerm](Lang.Unit)((rec, set) => Lang.Extend(rec, set).fix)

  override def headNorm = ???
