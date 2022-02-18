package cherry.lamr.norm
package umami

import cherry.fix.Fix
import cherry.lamr.{BuiltinType, Lang, LibRef, RecordKey, TypeOptions}
import cherry.utils.{Act, ErrorCtx, LayeredMap}

import scala.collection.immutable.TreeMap
import scala.collection.immutable.IntMap
import cherry.lamr.norm.umami.RecordValue.fromVector
import cats.syntax.traverseFilter.given
import tofu.syntax.collections.given
import tofu.syntax.foption.given

case class Abstract(term: Term, tpe: NormType) extends NormValue:
  def toTerm = term

  override def isAbstract = true

  private def make(term: Term, tpe: Process[NormType]): Process[NormValue] =
    tpe.map(Abstract(term, _))

  override def apply(arg: NormValue)                                              =
    make(term |> arg.toTerm, tpe.applied(arg))

  override def get(key: RecordKey, up: Int)                                       =
    make(term |> Lang.GetKey(key, up), tpe.got(key, up))

end Abstract

trait RecordValueBase extends NormValue:
  def map: LayeredMap[RecordKey, NormValue]

  def toTerm = joinAll(map.journal.iterator.map(toRecord))

  private def toRecord(key: RecordKey, value: NormValue): Term =
    Lang.set(key, value.toTerm)

  private def joinAll(it: IterableOnce[Term]): Term     =
    it.iterator.reduceOption((rec, set) => Lang.Merge(rec, set).fix).getOrElse(Lang.Unit)

  override def merge(term: NormValue): Process[NormValue]             = term match
    case ext: RecordValueBase => Act.pure(fromVector(map.journal ++ ext.map.journal))
    case _                    => super.merge(term)

  override def get(key: RecordKey, up: Int): Process[NormValue]       =
    val z = summon[ErrorCtx[State]]
    Act.option(map.get(key, up), Cause.MissingKey(key))

  protected def narrowField(
      domainMap: LayeredMap[RecordKey, NormType]
  )(name: RecordKey, fieldValue: NormValue): Process[Option[(RecordKey, NormValue)]] =
    domainMap.get(name, 0).traverse(fieldValue.narrow).mapIn(name -> _)

  override def narrow(domain: NormType): Process[NormValue]           =
    for
      ts  <- domain.fieldTypes
      dmap = LayeredMap.fromVector(ts)
      vs  <- map.journal.traverseFilter(narrowField(dmap))
    yield RecordValue.fromVector(vs)

end RecordValueBase

case class RecordValue(map: LayeredMap[RecordKey, NormValue]) extends RecordValueBase

object RecordValue:
  def single(key: RecordKey, v: NormValue) = fromVector(Vector(key -> v))

  def fromVector(kvs: Vector[(RecordKey, NormValue)]) = RecordValue(LayeredMap.fromVector(kvs))

  def from(kvs: (RecordKey, NormValue)*) = fromVector(kvs.toVector)

case class Closure(context: NormValue, body: Term, domain: NormType, norm: Normalizer) extends NormValue:
  def toTerm: Term = view(UnitValue)

  override def view(newContext: NormValue): Term =
    val func = Lang.Capture(domain.toTerm, body).fix
    if context == newContext then func else context.toTerm |> func

  override def apply(arg: NormValue)                     =
    for
      narrowArg   <- arg.narrow(domain)
      fullContext <- context.merge(narrowArg)
      res         <- norm.normalize(body, fullContext)
    yield res
end Closure

case class Merge(base: NormValue, ext: NormValue)                                             extends NormValue:
  def toTerm = Lang.Extend(base.toTerm, ext.toTerm).fix

  override def merge(ext2: NormValue) = ext.merge(ext2).flatMap(base.merge)

case class Narrow(base: NormValue, expect: NormType)                                          extends NormValue:
  def toTerm = Lang.Narrow(base.toTerm, expect.toTerm).fix

case object UnitValue                                                                         extends NormValue:
  def toTerm = Lang.Unit

  val pure = Act.pure(this)

  override def isUnit: Boolean = true

  override def narrow(domain: NormType): Process[NormValue] = pure

  override def merge(term: NormValue): Process[NormValue] = Act.pure(term)

trait BuiltinTypeValue(bt: BuiltinType) extends NormValue:
  override def narrow(domain: NormType): Process[NormValue] =
    domain match
      case BuiltinNormType(`bt`, _) => Act.pure(this)
      case _                        => super.narrow(domain)

case class Variable(id: Long, hint: String) extends NormValue:
  def toTerm = Lang.External(LibRef("variable", Lang.Integer(id)))

case class IntegerValue(value: BigInt)  extends BuiltinTypeValue(BuiltinType.Integer):
  def toTerm = Lang.Integer(value)

case class StringValue(value: String)   extends BuiltinTypeValue(BuiltinType.Str):
  def toTerm = Lang.Str(value)

case class FloatValue(value: Double)    extends BuiltinTypeValue(BuiltinType.Float):

  def toTerm = Lang.Float(value)

case class BooleanValue(value: Boolean) extends BuiltinTypeValue(BuiltinType.Bool):

  def toTerm = Lang.Bool(value)
