package cherry.lamr
import cherry.fix.Fix
import scala.language.dynamics

enum RecordKey:
  case Symbol(name: String)
  case Index(position: Int)

object RecordKey:

  given Conversion[String, RecordKey] = Symbol(_)
  given Conversion[Int, RecordKey]    = Index(_)

case class LibRef(pack: String, element: String)

case class TypeOptions()

enum Lang[+R]:
  case Record(name: RecordKey, typ: R)
  case Extend(base: R, deps: R)
  case Function(domain: R, body: R)
  case Type(options: TypeOptions)

  case Get(key: RecordKey, up: Int)
  case Unit
  case Id
  case Set(key: RecordKey, term: R)

  case Narrow(term: R, typ: R)

  case AndThen(left: R, right: R)
  case Capture(domain: R, body: R)
  case Apply

  case External(ref: LibRef)

  case Str(value: String)
  case Float(value: Double)
  case Integer(value: BigInt)
  case Bool(value: Boolean)

object Lang:
  extension [G[+x] >: Lang[x]](lang: Lang[Fix[G]]) def fix: Fix[G] = Fix(lang)

  given Conversion[String, Fix[Lang]]    = Str(_)
  given Conversion[scala.Int, Fix[Lang]] = Integer(_)
  given Conversion[Boolean, Fix[Lang]]   = Bool(_)

  def get(key: RecordKey) = Get(key, 0)

  object rec extends Dynamic:
    def applyDynamicNamed[G[+r] >: Lang[r]](name: "apply")(assocs: (String, Fix[G])*): Fix[G] =
      assocs
        .map((name, t) => Set(name, t).fix)
        .foldLeft[Fix[G]](Unit)((acc, t) => Extend(acc, t).fix)

    def applyDynamic[G[+r] >: Lang[r]](name: "apply")(assocs: Fix[G]*): Fix[G] =
      assocs.zipWithIndex
        .map((t, i) => Set(i, t).fix)
        .foldLeft[Fix[G]](Unit)((acc, t) => Extend(acc, t).fix)

  extension [G[+r] >: Lang[r]](term: Fix[G])
    infix def >>[A, H[+r] >: G[r]](next: Fix[H]): Fix[H] = Lang.AndThen(term, next).fix

    def apply[H[+r] >: G[r]](args: Fix[H]): Fix[H] = rec(term, args) >> Apply

type LangVal = Fix[Lang]
