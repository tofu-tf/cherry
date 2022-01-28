package cherry.lamr
import cherry.fix.Fix

enum RecordKey:
  case Symbol(name: String)
  case Index(position: Int)

case class LibRef(pack: String, element: String)

case class TypeOptions()

enum Lang[+R]:
  case Record(name: RecordKey, typ: R)
  case Extend(base: R, deps: R)
  case Function(domain: R, body: R)
  case Type(options: TypeOptions)

  case Get(key: RecordKey)
  case Unit
  case Set(key: RecordKey, term: R)

  case AndThen(left: R, right: R)
  case Capture(domain: R, body: R)
  case Expand(lam: R)

  case External(ref: LibRef)

  case Str(value: String)
  case Int(value: BigInt)
  case Bool(value: Boolean)

object Lang:
  extension (lang: Lang[Fix[Lang]]) def fix: Fix[Lang] = Fix(lang)

type LangVal = Fix[Lang]
