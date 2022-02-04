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
import cherry.lamr.BuiltinType

trait NormType                                               extends NormValue:
  def fieldTypes(domain: NormValue): Process[Vector[(RecordKey, NormType)]] = Act.pure(Vector.empty)

  def asAbstract: Process[NormValue] = ???

case class BuiltinNormType(bt: BuiltinType)                  extends NormType:
  def toPartial = Lang.Builtin(bt)

case class UniverseType(options: TypeOptions)                extends NormType:
  def toPartial = Lang.Universe(options)

case class RecordType(fields: Vector[(RecordKey, NormType)]) extends NormType:
  def toPartial = fields.iterator.map((k, v) => Lang.Record(k, v.toPartial).fix).reduce(Lang.Extend(_, _).fix)

case class FunctionType(dom: NormType, body: NormType)       extends NormType:
  def toPartial = Lang.Function(dom.toPartial, body.toPartial).fix

object RecordType:
  def single(key: RecordKey, fieldType: NormType) = RecordType(Vector(key -> fieldType))

case class ExtendType(base: NormType, ext: NormType) extends NormType:
  def toPartial = Lang.Extend(base.toPartial, ext.toPartial).fix
