package cherry.lamr.norm
package umami

import cherry.fix.{Fix, Traverse}
import cherry.lamr.{BuiltinType, Lang, LibRef, RecordKey, TypeOptions}
import cherry.utils.{Act, Raising, LayeredMap}

import scala.collection.immutable.TreeMap
import scala.collection.immutable.IntMap
import cherry.lamr.norm.umami.RecordValue.fromVector
import cherry.fix.Functor
import cherry.fix.Functor.{given Functor[Vector], given Functor[Option]}

case class Abstract(term: Term, tpe: NormType) extends NormValue:
  def toTerm = Process.pure(term)

  override def isAbstract = true

  private def make(term: Process[Term], tpe: Process[NormType]): Process[NormValue] =
    term.map2Par(tpe)(Abstract(_, _))

  override def apply(arg: NormValue) =
    make(arg.toTerm.map(term |> _), tpe.applied(arg))

  override def get(key: RecordKey, up: Int) =
    make(Act.pure(term |> Lang.GetKey(key, up)), tpe.got(key, up))

end Abstract

trait RecordValueBase extends NormValue:
  def map: LayeredMap[RecordKey, NormValue]

  def toTerm = map.journal.traverse(toRecord).map(joinAll)

  private def toRecord(key: RecordKey, value: NormValue): Process[Term] =
    value.toTerm.map(Lang.set(key, _))

  private def joinAll(it: IterableOnce[Term]): Term =
    it.iterator.reduceOption((rec, set) => Lang.Merge(rec, set).fix).getOrElse(Lang.Unit)

  override def merge(term: NormValue): Process[NormValue] = term match
    case ext: RecordValueBase => Act.pure(fromVector(map.journal ++ ext.map.journal))
    case _                    => super.merge(term)

  override def get(key: RecordKey, up: Int): Process[NormValue] =
    Act.option(map.get(key, up), Cause.MissingKey(key))

  protected def narrowField(
      domainMap: LayeredMap[RecordKey, NormType]
  )(name: RecordKey, fieldValue: NormValue): Process[Option[(RecordKey, NormValue)]] =
    domainMap.get(name, 0).traverse(fieldValue.narrow).map(_.map(name -> _))

  override def narrow(domain: NormType): Process[NormValue] =
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

case class Closure(context: NormValue, body: Process[NormValue], domain: NormType) extends NormValue:

  private def rebuild: Process[NormValue] =
    for
      abs  <- domain.asAbstract
      full <- context.merge(abs)
      res  <- body.locally(_.context = full)
    yield res

  private def view: Process[NormValue] =
    Process.context.flatMap(newCtx => if context == context then body else rebuild)

  def toTerm: Process[Term] =
    for
      dom  <- domain.toTerm
      body <- view >>= (_.toTerm)
    yield Lang.Capture(dom, body).fix

  override def apply(arg: NormValue) =
    for
      narrowArg   <- arg.narrow(domain)
      fullContext <- context.merge(narrowArg)
      res         <- body.locally(_.context = fullContext)
    yield res
end Closure

case class Merge(base: NormValue, ext: NormValue) extends NormValue:
  def toTerm = base.toTerm.parMap2(ext.toTerm)(Lang.Extend(_, _).fix)

  override def merge(ext2: NormValue) = ext.merge(ext2).flatMap(base.merge)

case class Narrow(base: NormValue, expect: NormType) extends NormValue:
  def toTerm = base.toTerm.map2Par(expect.toTerm)(Lang.Narrow(_, _).fix)

case object UnitValue extends NormValue:
  val toTerm = Process.pure(Lang.Unit)

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
  def toTerm = Process.pure(Lang.External(LibRef("variable", Lang.Integer(id))))

case class IntegerValue(value: BigInt) extends BuiltinTypeValue(BuiltinType.Integer):
  def toTerm = Process.pure(Lang.Integer(value))

case class StringValue(value: String) extends BuiltinTypeValue(BuiltinType.Str):
  def toTerm = Process.pure(Lang.Str(value))

case class FloatValue(value: Double) extends BuiltinTypeValue(BuiltinType.Float):

  def toTerm = Process.pure(Lang.Float(value))

case class BooleanValue(value: Boolean) extends BuiltinTypeValue(BuiltinType.Bool):

  def toTerm = Process.pure(Lang.Bool(value))
