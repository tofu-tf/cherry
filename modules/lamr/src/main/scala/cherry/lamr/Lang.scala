package cherry.lamr
import cherry.fix.Fix

enum RecordKey:
  case Symbol(name: String)
  case Index(position: Int)

case class LibRef(pack: String, element: String)

case class TypeOptions()

enum Lang[+R]:
  case Record(fields: Map[RecordKey, R])
  case Merge(base: R, deps: R)
  case Function(body: R)
  case Type(options: TypeOptions)
  
  case Get(key: RecordKey)
  case Create(record: Map[RecordKey, R])

  case AndThen(left: R, right: R)
  case Capture(domain: R, body: R)
  case Apply(lam: R)

  case External(ref: LibRef)

  case Str(value: String)
  case Int(value: BigInt)
  case Bool(value: Boolean)

type LangVal = Fix[Lang]
