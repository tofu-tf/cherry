package cherry.lamr.norm
package umami

import cherry.fix.Fix
import cherry.lamr.{BuiltinType, Lang, RecordKey, TypeOptions}
import cherry.utils.{Act, LayeredMap}
import tofu.syntax.parallel.given

import scala.collection.immutable.{IntMap, TreeMap}

trait NormType                                                            extends NormValue:
  def fieldTypes: Process[Vector[(RecordKey, NormType)]] = Act.pure(Vector.empty)

  def asAbstract: Process[NormValue] = Act.pure(Abstract(Lang.Id, this))

  def applied(arg: NormValue): Process[NormType] = Act.error(Cause.BadType(TypeCause.Function))

  def got(key: RecordKey, up: Int): Process[NormType] = Act.error(Cause.BadType(TypeCause.Record))

  override def asType: Process[NormType] = Act.pure(this)

case class BuiltinNormType(bt: BuiltinType, ext: Option[NormType] = None) extends NormType:
  override def toTerm: Term = Lang.Builtin(bt)

case class UniverseType(options: TypeOptions)                             extends NormType:
  override def toTerm: Term = Lang.Universe(options)

case class RecordType(fields: LayeredMap[RecordKey, NormType])            extends NormType:
  def toTerm: Term                                             =
    fields.journal.map((k, v) => Lang.Record(k, v.toTerm, TypeOptions()).fix).reduce(Lang.Extend(_, _).fix)

  override def asAbstract                                      =
    fields.parTraverse(t => t.asAbstract).map(kvs => RecordValue.fromVector(kvs.journal))

  override def got(key: RecordKey, up: Int): Process[NormType] =
    Act.option(fields.get(key, up), Cause.MissingKey(key))

  override def fieldTypes                                      = Act.pure(fields.journal)

case class FunctionType(dom: NormType, body: NormType)                    extends NormType:
  def toTerm = Lang.Function(dom.toTerm, body.toTerm).fix

object RecordType:
  def single(key: RecordKey, fieldType: NormType) = RecordType.fromVector(Vector(key -> fieldType))

  def fromVector(kvs: Vector[(RecordKey, NormType)]) = RecordType(LayeredMap.fromVector(kvs))

case class ExtendType(base: NormType, ext: NormType) extends NormType:
  def toTerm = Lang.Extend(base.toTerm, ext.toTerm).fix
