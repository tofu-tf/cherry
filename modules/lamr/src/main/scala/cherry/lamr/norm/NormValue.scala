package cherry.lamr.norm

import cherry.lamr.{Lang, LibRef}

import cherry.fix.Fix
import tofu.syntax._
import cats.syntax.show._

trait Normalizer:
  def bigStep(term: PartialTerm, context: PartialTerm): Process[PartialTerm]

trait NormValue:
  def toPartial: PartialTerm

  def errorDisplay: Option[String] = Some(toPartial.toString)

  def position: Option[Position] = None

  def error(message: String) = Error(message, Some(toPartial), position).raise

  def apply(term: NormValue): Process[NormValue] = error("not applicable")

  def asInt: Process[BigInt] = error("not an int")

  def asFloat: Process[Double] = error("not a float")

  def asString: Process[String] = error("not a string")

  def getKey: Process[NormValue] = error("key ")

type Partial[+A] = Lang[A] | Symbol

type PartialTerm = Fix[Partial]

trait Library:
  def resolve(context: PartialTerm, position: Position, ref: LibRef, normalizer: Normalizer): Process[PartialTerm]

// class LibraryPack(includes: Map[String, Library]) extends Library:
// end LibraryPack

// class UmamiNormalizer(library: Library) extends Normalizer:

// end UmamiNormalizer
