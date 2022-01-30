package cherry.lamr.norm
package umami

import cherry.fix.Fix
import cherry.lamr.Lang
import cherry.utils.Act
import cherry.lamr.RecordKey
import scala.collection.immutable.TreeMap
import scala.collection.immutable.IntMap

case class Abstract(term: PartialTerm, val posistion: Option[Position] = None) extends NormValue:
  def toPartial = term

  def apply(arg: PartialTerm) =
    Abstract(Fix(Lang.AndThen(term, arg)), posistion)
class OrderedMap[K, +V](items: Map[K, (V, Int)], order: IntMap[K])             extends Map[K, V]:
  def iterator                      = order.valuesIterator.map(k => k -> items(k)._1)
  def get(k: K)                     = items.get(k).map(_._1)
  def removed(k: K)                 = items.get(k) match
    case Some((_, i)) => OrderedMap(items - k, order - i)
    case _            => this
  def updated[V1 >: V](k: K, v: V1) =
    val idx = if isEmpty then 0 else order.lastKey + 1
    OrderedMap(items + (k -> (v, idx)), order + (idx -> k))

end OrderedMap
// export order.values
// export map.get
// export removed(k: K) =
//   val idx = indices(k)

// def

// case class Record(items: OrderedMap[RecordKey, PartialTerm]) extends NormValue
