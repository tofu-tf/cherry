package cherry.lamr

enum RecordKey:
  case Symbol(name: String)
  case Index(position: Int)

case class LibRef(pack: String, element: String)

sealed trait Lang[+R]

enum Types[+R] extends Lang[R]:
  case Record(fields: Map[RecordKey, R])
  case Flow(domain: R, result: R)
  case Type

enum Basic[+R] extends Lang[R]:
  case Get(key: RecordKey)
  case Create(record: Map[RecordKey, R])
  case AndThen(left: R, right: R)
  case External(ref: LibRef)

enum Closure[+R] extends Lang[R]:
  case Lambda(domain: R, body: R)
  case Apply(lam: R, args: R)


