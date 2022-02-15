package cherry.lamr
import cherry.fix.Fix

import scala.annotation.targetName
import scala.language.dynamics

enum RecordKey:
  case Symbol(name: String)
  case Index(position: Int)

object RecordKey:

  given Conversion[String, RecordKey] = Symbol(_)
  given Conversion[Int, RecordKey]    = Index(_)

case class LibRef(pack: String, element: Fix[Lang])

case class TypeOptions(
    infer: Boolean = false,
    erase: Boolean = false,
)

enum BuiltinType:
  case Integer, Float, Str, Bool, Any

enum Lang[+R]:
  case Universe(options: TypeOptions)

  case Record(name: RecordKey, typ: R)
  case Extend(base: R, deps: R)
  case Function(domain: R, body: R)
  case Builtin(bt: BuiltinType)

  case GetKey(key: RecordKey, up: Int)
  case Unit
  case Id
  case Set(key: RecordKey, term: R, options: TypeOptions)
  case Merge(base: R, deps: R)

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

  object get extends Dynamic:
    def apply(key: RecordKey) = GetKey(key, 0)

    def unapply(key: GetKey[Any]): Option[RecordKey] = Option.when(key.up == 0)(key.key)

    def selectDynamic(name: String) = GetKey(name, 0)

  def set[G[+r] >: Lang[r]](key: RecordKey, t: Fix[G]): Fix[G] = Set(key, t, TypeOptions()).fix

  object rec extends Dynamic:
    def applyDynamicNamed[G[+r] >: Lang[r]](name: "apply")(assocs: (String, Fix[G])*): Fix[G] =
      assocs
        .map((name, t) => set(name, t))
        .reduceOption(Merge(_, _).fix)
        .getOrElse(Unit)

    def applyDynamic[G[+r] >: Lang[r]](name: "apply")(assocs: Fix[G]*): Fix[G] =
      assocs.zipWithIndex
        .map((t, i) => set(i, t))
        .reduceOption(Merge(_, _).fix)
        .getOrElse(Unit)

  extension [G[+r] >: Lang[r]](term: Fix[G])
    @targetName("andThen")
    infix def |>[A, H[+r] >: G[r]](next: Fix[H]): Fix[H] = Lang.AndThen(term, next).fix

    def apply[H[+r] >: G[r]](args: Fix[H]): Fix[H] = rec(term, args) |> Apply

    def merge[H[+r] >: G[r]](ext: Fix[H]): Fix[H] = Merge(term, ext).fix

type LangVal = Fix[Lang]
