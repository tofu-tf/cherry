package cherry.lamr.norm.floats

import cherry.lamr.norm.umami.FloatValue
import cherry.lamr.{Lang, LibRef, RecordKey}
import cherry.lamr.norm.{Cause, Library, NameResolutionLibrary, NormValue, Normalizer, Process, Term}
import cherry.utils.Act

object FloatsLibrary extends NameResolutionLibrary("floats"):
  val members = {
    case "plus"  => Fn("plus", _ + _)
    case "minus" => Fn("minus", _ - _)
    case "div"   => Fn("div", _ / _)
    case "mul"   => Fn("mul", _ * _)
  }

  class Fn(name: String, call: (Double, Double) => Double) extends NormValue:
    override def toTerm = Process.pure(Lang.External(LibRef("floats", Lang.get(name))))

    override def apply(term: NormValue): Process[NormValue] =
      term.first.flatMap(_.asDouble).map2Par(term.second.flatMap(_.asDouble))((x, y) => FloatValue(call(x, y)))
