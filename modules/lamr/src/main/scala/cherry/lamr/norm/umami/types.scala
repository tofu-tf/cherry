package cherry.lamr.norm
package umami

import cherry.fix.Fix
import cherry.lamr.{BuiltinType, Lang, RecordKey, TypeOptions}
import cherry.utils.{Act, LayeredMap}

import scala.collection.immutable.{IntMap, TreeMap}
import cherry.fix.TraverseFilter
import cherry.fix.Functor.{given TraverseFilter[Vector]}

trait NormType extends NormValue:
  def fieldTypes: Process[Vector[(RecordKey, NormType)]] = Act.pure(Vector.empty)

  def asAbstract: Process[NormValue] = Act.pure(Abstract(Lang.Id, this))

  def applied(arg: NormValue): Process[NormType] = Act.error(Cause.BadType(TypeCause.Function))

  def got(key: RecordKey, up: Int): Process[NormType] = Act.error(Cause.BadType(TypeCause.Record))

  override def asType: Process[NormType] = Act.pure(this)

case class BuiltinNormType(bt: BuiltinType, ext: Option[NormType] = None) extends NormType:
  override def toTerm = Process.pure(Lang.Builtin(bt))

case class UniverseType(options: TypeOptions) extends NormType:
  override def toTerm = Process.pure(Lang.Universe(options))

case class RecordType(fields: LayeredMap[RecordKey, NormType]) extends NormType:
  def toTerm =
    fields.journal
      .parTraverse((k, v) => v.toTerm.map(Lang.Record(k, _, TypeOptions()).fix))
      .map(_.reduce(Lang.Extend(_, _).fix))

  override def asAbstract =
    fields.parTraverse(t => t.asAbstract).map(kvs => RecordValue.fromVector(kvs.journal))

  override def got(key: RecordKey, up: Int): Process[NormType] =
    Act.option(fields.get(key, up), Cause.MissingKey(key))

  override def fieldTypes = Act.pure(fields.journal)

case class FunctionType(dom: NormType, effect: NormType, result: NormType) extends NormType:
  def toTerm = (dom.toTerm, effect.toTerm, result.toTerm).parMapN(Lang.Function(_, _, _).fix)

object RecordType:
  def single(key: RecordKey, fieldType: NormType) = RecordType.fromVector(Vector(key -> fieldType))

  def fromVector(kvs: Vector[(RecordKey, NormType)]) = RecordType(LayeredMap.fromVector(kvs))

case class ExtendType(base: NormType, ext: NormType) extends NormType:
  def toTerm = base.toTerm.parMap2(ext.toTerm)(Lang.Extend(_, _).fix)
