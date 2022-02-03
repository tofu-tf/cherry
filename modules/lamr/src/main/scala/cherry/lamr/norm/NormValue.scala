package cherry.lamr.norm

import cherry.lamr.{Lang, LibRef}

import cherry.fix.Fix
import tofu.syntax._
import cats.syntax.show._
import cherry.utils.Act
import cherry.lamr.RecordKey

case class Symbol(id: Long, name: String)

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

  def apply(term: NormValue): Process[NormValue] = error(Cause.BadType("function"))

  def get(key: RecordKey): Process[NormValue] = error(Cause.BadType("record"))

  def extend(term: NormValue): Process[NormValue] = ???

  def narrow(domain: NormValue): Process[NormValue] = ???

  def fieldTypes(domain: NormValue): Process[NormValue] = ???

  final def error(cause: Cause) = Error(cause, Some(toPartial), position).raise

trait Library:
  def resolve(context: PartialTerm, position: Position, ref: LibRef, normalizer: Normalizer): Process[PartialTerm]

class LibraryPack(includes: Map[String, Library]) extends Library:
  def resolve(context: PartialTerm, position: Position, ref: LibRef, normalizer: Normalizer) =
    for
      lib  <- Act.option(includes.get(ref.pack), Error(Cause.MissingLibrary(ref.pack), None, Some(position)))
      term <- lib.resolve(context, position, ref, normalizer)
    yield term

end LibraryPack
