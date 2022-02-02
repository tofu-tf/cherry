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

  case Get(key: RecordKey)
  case Unit
  case Id
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

  given Conversion[String, Fix[Lang]]    = Str(_)
  given Conversion[scala.Int, Fix[Lang]] = Int(_)
  given Conversion[Boolean, Fix[Lang]]   = Bool(_)

  object rec extends Dynamic:
    def applyDynamicNamed(name: "apply")(assocs: (String, Fix[Lang])*): Fix[Lang] =
      assocs
        .map((name, t) => Set(name, t).fix)
        .foldLeft[Fix[Lang]](Unit)((acc, t) => Extend(acc, t).fix)

    def applyDynamic(name: "apply")(assocs: Fix[Lang]*): Fix[Lang] =
      assocs.zipWithIndex
        .map((t, i) => Set(i, t).fix)
        .foldLeft[Fix[Lang]](Unit)((acc, t) => Extend(acc, t).fix)

  extension (term: Fix[Lang])
    def apply(args: Fix[Lang]): Fix[Lang] =
      Lang.AndThen(Extend(Id, args).fix, Expand(term).fix).fix

type LangVal = Fix[Lang]
