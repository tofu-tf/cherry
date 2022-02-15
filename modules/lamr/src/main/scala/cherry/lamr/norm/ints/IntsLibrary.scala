package cherry.lamr.norm.ints
import cherry.lamr.norm.umami.IntegerValue
import cherry.lamr.{Lang, LibRef, RecordKey}
import cherry.lamr.norm.{Cause, Library, NameResolutionLibrary, NormValue, Normalizer, PartialTerm, Process}
import cherry.utils.Act

object IntsLibrary extends NameResolutionLibrary("ints"):
  val members = {
    case "plus"   => Fn("plus", _ + _)
    case "minus"  => Fn("minus", _ - _)
    case "div"    => Fn("div", _ / _)
    case "mul"    => Fn("mul", _ * _)
    case "xor"    => Fn("xor", _ ^ _)
    case "bitand" => Fn("bitand", _ & _)
    case "bitor"  => Fn("bitor", _ | _)
    case "gcd"    => Fn("gcd", _ gcd _)
  }

  class Fn(name: String, call: (BigInt, BigInt) => BigInt) extends NormValue:
    override def toPartial: PartialTerm = Lang.External(LibRef("ints", Lang.get(name)))

    override def apply(term: NormValue): Process[NormValue] =
      term.first.flatMap(_.asInt).map2Par(term.second.flatMap(_.asInt))((x, y) => IntegerValue(call(x, y)))
