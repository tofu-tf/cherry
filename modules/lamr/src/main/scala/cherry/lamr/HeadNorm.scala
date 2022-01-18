package cherry.lamr

import cherry.fix.Fix
import cats.data.EitherK
import scala.annotation.unchecked.uncheckedVariance
import java.text.Normalizer

enum Res[+R]:
  case Error(message: String, other: Vector[String] = Vector.empty)
  case Stop
  case Ok(res: R)
final case class TypeInfo(
  typeTerm: LangVal
)

trait Normal:
  def resolve(ref: LibRef, normalizer: Normalizer): Res[NormVal]

// class Normalizer(packs: Map[String, NormPack]):
//   def headNorm(lang: NormVal, context: Map[RecordKey, NormVal]): Res[LangVal] = 
//     lang.unpack match 
//       case Types.Field(name, c) => 
