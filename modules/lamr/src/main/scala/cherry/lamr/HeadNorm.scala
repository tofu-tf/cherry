package cherry.lamr

import cherry.fix.Fix
import cats.data.EitherK
import scala.annotation.unchecked.uncheckedVariance
import java.text.Normalizer

trait Custom[+R]:
  def maybeNorm: Fix[Lang]
  def apply(context: NormVal, arg: NormVal): Either[String, NormVal]
  def andThen(pre: NormVal): Either[String, NormVal]

type NormV[+A] = EitherK[Custom, Lang, A @uncheckedVariance]
type NormVal   = Fix[NormV]

trait NormPack:
  def resolve(ref: LibRef): Either[String, NormVal]

class Normalizer(packs: Map[String, NormPack]):
  def headNorm(lang: Fix[Lang]): Either[String, NormVal] = ???
