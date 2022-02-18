package cherry.lamr.norm

import cherry.lamr.{Lang, LibRef, RecordKey}
import cherry.utils.{Act, LayeredMap}
import cats.kernel.Monoid
import cherry.lamr.norm.ints.IntsLibrary
import cherry.lamr.norm.umami.{RecordValue, RecordValueBase}

trait Library:
  def resolve(context: NormValue, ref: LibRef, normalizer: Normalizer): Process[NormValue]

class LibraryPack(includes: Map[String, Library]) extends Library:
  def resolve(context: NormValue, ref: LibRef, normalizer: Normalizer) =
    for
      lib  <- Act.option(includes.get(ref.pack), Cause.MissingLibrary(ref.pack))
      term <- lib.resolve(context, ref, normalizer)
    yield term

end LibraryPack

private def builtins          = Vector[NameResolutionLibrary](IntsLibrary)
private def builtinMap        = builtins.iterator.map(bi => bi.name -> bi).toMap
private def builtinLayeredMap = LayeredMap.fromVector(builtins.map(bi => (bi.name: RecordKey) -> bi))

object BuiltinLibrary extends LibraryPack(builtinMap) with RecordValueBase:
  val map = builtinLayeredMap

trait NameResolutionLibrary(val name: String) extends Library with NormValue:

  override def toPartial: Term = Lang.External(LibRef(name, Lang.get(0)))

  override def resolve(context: NormValue, ref: LibRef, normalizer: Normalizer): Process[NormValue] =
    ref.element.unpack match
      case Lang.get(key @ (RecordKey.Index(0) | _: RecordKey.Symbol)) => getKey(key)
      case _                                                          => Act.error(Cause.BadRef(ref))

  def members: PartialFunction[String, NormValue]

  override def get(key: RecordKey, up: Int): Process[NormValue] =
    if up == 0 then getKey(key) else Act.error(Cause.MissingKey(key))

  private def getKey(key: RecordKey): Process[NormValue]        = key match
    case RecordKey.Symbol(name) => Act.option(members.lift(name), Cause.MissingKey(key))
    case RecordKey.Index(0)     => Act.pure(this)
    case _                      => Act.error(Cause.MissingKey(key))
