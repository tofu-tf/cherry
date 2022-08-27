package cherry.lamr
import cherry.fix.{Fix, Traverse}
import cherry.fix.Fix.Fix
import cherry.lamr.Lang.{Capture, External, GetKey, Universe}
import cherry.utils.SimpleTraversing

import scala.annotation.targetName
import scala.language.dynamics

enum RecordKey:
  case Symbol(name: String)
  case Index(position: Int)

object RecordKey:

  given Conversion[String, RecordKey] = Symbol(_)
  given Conversion[Int, RecordKey]    = Index(_)

case class LibRef(pack: String, element: Term)

case class TypeOptions(
    infer: Boolean = false,
    erase: Boolean = false,
)

object TypeOptions:
  val Default = TypeOptions()

enum BuiltinType:
  case Integer, Float, Str, Bool, Any

enum Lang[+R] derives Traverse:
  case Universe(options: TypeOptions) extends Lang[Nothing]

  case Record(name: RecordKey, typ: R, options: TypeOptions)
  case Extend(base: R, deps: R)

  case Function(domain: R, effect: R, body: R)
  case Builtin(bt: BuiltinType) extends Lang[Nothing]

  case GetKey(key: RecordKey, up: Int) extends Lang[Nothing]
  case Unit
  case Id
  case Set(key: RecordKey, term: R)
  case Merge(base: R, deps: R)

  case Narrow(term: R, typ: R)

  case AndThen(left: R, right: R)

  case Capture(domain: R, body: R)
  case Apply

  case External(ref: LibRef) extends Lang[Nothing]

  case Str(value: String)     extends Lang[Nothing]
  case Float(value: Double)   extends Lang[Nothing]
  case Integer(value: BigInt) extends Lang[Nothing]
  case Bool(value: Boolean)   extends Lang[Nothing]

type Term = Fix[Lang]

object Lang:
  extension [G[+x] >: Lang[x]](lang: Lang[Fix[G]]) def fix: Fix[G] = Fix(lang)

  given Conversion[String, Fix[Lang]]    = Str(_)
  given Conversion[scala.Int, Fix[Lang]] = Integer(_)
  given Conversion[Boolean, Fix[Lang]]   = Bool(_)

  val U = Universe(TypeOptions.Default)

  object get extends Dynamic:
    def apply(key: RecordKey) = GetKey(key, 0)

    def unapply(key: GetKey): Option[RecordKey] = Option.when(key.up == 0)(key.key)

    def selectDynamic(name: String) = GetKey(name, 0)

  def set[G[+r] >: Lang[r]](key: RecordKey, t: Fix[G]): Fix[G] = Set(key, t).fix

  object rec extends Dynamic:
    def applyDynamicNamed(name: "apply")(assocs: (String, Term)*): Term =
      assocs
        .map((name, t) => set(name, t))
        .reduceOption(Merge(_, _).fix)
        .getOrElse(Unit)

    def applyDynamic(name: "apply")(assocs: Term*): Term =
      assocs.zipWithIndex
        .map((t, i) => set(i, t))
        .reduceOption(Merge(_, _).fix)
        .getOrElse(Unit)

  object recT extends Dynamic:
    def applyDynamicNamed(name: "apply")(assocs: (String, Term)*): Term =
      assocs
        .map((name, t) => Record(name, t, TypeOptions()).fix)
        .reduceOption(Extend(_, _).fix)
        .getOrElse(Unit)

  class Call(term: Term) extends Dynamic:
    def applyDynamicNamed(name: "apply")(assocs: (String, Term)*): Term =
      term.apply(rec.applyDynamicNamed("apply")(assocs*))

  extension (term: Fix[Lang])
    infix def |>(next: Term): Term = Lang.AndThen(term, next).fix

    infix def -->(body: Term): Term = term -- BuiltinType.Any --> body

    infix def --(eff: Term) = ArrowBuilder(term, eff)

    def andThen(next: Term): Term = Lang.AndThen(term, next).fix

    def apply(args: Term): Term = rec(term, args) |> Apply

    def lam(body: Term): Term = Capture(term, body).fix

    def merge(ext: Term): Term = Merge(term, ext).fix

    def call: Call = Call(term)

  class ArrowBuilder(domain: Term, eff: Term):
    def -->(body: Term): Term = Lang.Function(domain, eff, body).fix

  given Conversion[BuiltinType, Term] = Lang.Builtin(_)

type LangVal = Fix[Lang]
