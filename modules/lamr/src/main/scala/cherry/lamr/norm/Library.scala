package cherry.lamr.norm

import cherry.lamr.LibRef
import cherry.utils.Act
import cats.kernel.Monoid

trait Library:
  def resolve(context: NormValue, ref: LibRef, normalizer: Normalizer): Process[NormValue]


class LibraryPack(includes: Map[String, Library]) extends Library:
  def resolve(context: NormValue, ref: LibRef, normalizer: Normalizer) =
    for
      lib  <- Act.option(includes.get(ref.pack), Cause.MissingLibrary(ref.pack))
      term <- lib.resolve(context, ref, normalizer)
    yield term

end LibraryPack

object BuiltinLibrary extends Library:
    def resolve(context: NormValue, ref: LibRef, normalizer: Normalizer): Process[NormValue] = ???
