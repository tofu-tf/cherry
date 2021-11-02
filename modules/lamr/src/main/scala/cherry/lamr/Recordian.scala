package cherry.lamr

enum RecordKey:
  case Symbol(name: String)
  case Index(position: Int)

case class LibRef(pack: String, element: String)

enum Types[+R]:
  case Record(fields: Map[RecordKey, R])
  case Flow(domain: R, result: R)
  case Type

enum Lang[+R]:
  case Get(key: RecordKey)
  case Create(record: Map[RecordKey, R])
  case AndThen(left: R, right: R)
  case Rename(perm: Map[RecordKey, RecordKey])
  case External(ref: LibRef)

enum Closure[+R]:
  case Lambda(args: R, body: R)
  case Apply(lam: R, args: R)
