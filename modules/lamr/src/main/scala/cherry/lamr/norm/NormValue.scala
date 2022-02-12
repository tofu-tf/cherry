package cherry.lamr.norm

import cherry.lamr.{Lang, LibRef}
import cherry.fix.Fix
import tofu.syntax.*
import cats.syntax.show.*
import cherry.utils.Act
import cherry.lamr.RecordKey
import cherry.lamr.norm.umami.{Narrow, NormType}

case class Symbol[+T](id: Long, key: RecordKey, tpe: T)

type Partial[+A] = Lang[A] | Symbol[A]

type PartialTerm = Fix[Partial]
trait Normalizer:
  def bigStep(term: PartialTerm, context: NormValue): Process[NormValue]

trait NormValue:
  def toPartial: PartialTerm

  def headNorm: Process[NormValue] = Act.pure(this)

  def errorDisplay: Option[String] = Some(toPartial.toString)

  def position: Option[Position] = None

  def isAbstract: Boolean = false

  def apply(term: NormValue): Process[NormValue] = Cause.BadType(TypeCause.Function).raise

  def get(key: RecordKey, up: Int): Process[NormValue] = Cause.BadType(TypeCause.Record).raise

  def asType: Process[NormType] = Cause.BadType(TypeCause.Type).raise

  def merge(term: NormValue): Process[NormValue] = Act.pure(umami.Merge(this, term))

  def narrow(domain: NormType): Process[NormValue] = Act.error(Cause.UnrelatedValue)

  def first = get(0, 0)

  def second = get(1, 0)

trait Library:
  def resolve(context: NormValue, ref: LibRef, normalizer: Normalizer): Process[NormValue]

class LibraryPack(includes: Map[String, Library]) extends Library:
  def resolve(context: NormValue, ref: LibRef, normalizer: Normalizer) =
    for
      lib  <- Act.option(includes.get(ref.pack), Cause.MissingLibrary(ref.pack))
      term <- lib.resolve(context, ref, normalizer)
    yield term

end LibraryPack
