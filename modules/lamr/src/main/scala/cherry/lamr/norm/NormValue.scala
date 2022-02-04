package cherry.lamr.norm

import cherry.lamr.{Lang, LibRef}

import cherry.fix.Fix
import tofu.syntax._
import cats.syntax.show._
import cherry.utils.Act
import cherry.lamr.RecordKey
import cherry.lamr.norm.umami.NormType

case class Symbol(id: Long, key: RecordKey)

type Partial[+A] = Lang[A] | Symbol

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

  def merge(term: NormValue): Process[NormValue] = ???

  def narrow(domain: NormValue): Process[NormValue] = ???

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
