package cherry.lamr.norm.strs
import cherry.lamr.norm.umami.StringValue
import cherry.lamr.{Lang, LibRef, RecordKey}
import cherry.lamr.norm.{Cause, Library, NameResolutionLibrary, NormValue, Normalizer, Process, Term}
import cherry.utils.Act

object StringsLibrary extends NameResolutionLibrary("strs"):
  val members = { case "plus" =>
    Fn("plus", _ + _)
  }

  class Fn(name: String, call: (String, String) => String) extends NormValue:
    override def toTerm = Process.pure(Lang.External(LibRef("strs", Lang.get(name))))

    override def apply(term: NormValue): Process[NormValue] =
      term.first.flatMap(_.asStr).map2Par(term.second.flatMap(_.asStr))((x, y) => StringValue(call(x, y)))
