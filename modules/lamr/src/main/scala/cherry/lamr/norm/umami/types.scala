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

trait NormType                                               extends NormValue:
  def fieldTypes(domain: NormValue): Process[Vector[(RecordKey, NormType)]] = Act.pure(Vector.empty)
  
  def asAbstract: Process[NormValue] = ???

case class TypeValue(options: TypeOptions)                   extends NormType:
  def toPartial = Lang.Type(options)

case class RecordType(fields: Vector[(RecordKey, NormType)]) extends NormType:
  def toPartial = fields.iterator.map((k, v) => Lang.Record(k, v.toPartial).fix).reduce(Lang.Extend(_, _).fix)

object RecordType:
  def single(key: RecordKey, fieldType: NormType) = RecordType(Vector(key -> fieldType))

case class ExtendType(base: NormType, ext: NormType) extends NormType:
  def toPartial = Lang.Extend(base.toPartial, ext.toPartial).fix
