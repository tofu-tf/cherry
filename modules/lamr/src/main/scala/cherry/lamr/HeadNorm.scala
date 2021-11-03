package cherry.lamr

import cherry.fix.Fix
import cats.data.EitherK
import scala.annotation.unchecked.uncheckedVariance
import java.text.Normalizer

enum Res:
  case Error(message: String, other: Vector[String] = Vector.empty)
  case Stop
  case Ok(res: NormVal)

trait Custom[+R]:
  def maybeNorm: Fix[Lang]
  def apply(context: NormVal, arg: NormVal): Res
  def andThen(pre: NormVal): Res

type NormV[+A] = EitherK[Custom, Lang, A @uncheckedVariance]
type NormVal   = Fix[NormV]

trait NormPack:
  def resolve(ref: LibRef): Res

class Normalizer(packs: Map[String, NormPack]):
  def headNorm(lang: Fix[Lang]): Res = ???
