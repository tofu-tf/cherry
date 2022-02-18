package cherry.lamr
import cats.{Applicative, Eval, Traverse}
import cherry.fix.Fix
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

case class LibRef(pack: String, element: Fix[Lang])

case class TypeOptions(
    infer: Boolean = false,
    erase: Boolean = false,
)

object TypeOptions:
  val Default = TypeOptions()

enum BuiltinType:
  case Integer, Float, Str, Bool, Any

  given Conversion[BuiltinType, Lang[Nothing]] = Lang.Builtin(_)

enum Lang[+R] extends SimpleTraversing[Lang, R]:
  def traverse[F[_], X](f: R => F[X])(using F: Applicative[F]): F[Lang[X]] = this match
    case u @ (
          _: Universe | _: Builtin | Id | Unit | Apply | _: Str | _: Float | _: Integer | _: Bool | _: External |
          _: GetKey
        ) =>
      F.pure(u)
    case Extend(base, deps)         => F.map2(f(base), f(deps))(Extend(_, _))
    case Function(domain, body)     => F.map2(f(domain), f(body))(Function(_, _))
    case Set(key, term)             => F.map(f(term))(Set(key, _))
    case Merge(base, deps)          => F.map2(f(base), f(deps))(Merge(_, _))
    case Narrow(term, typ)          => F.map2(f(term), f(typ))(Narrow(_, _))
    case AndThen(left, right)       => F.map2(f(left), f(right))(AndThen(_, _))
    case Capture(domain, body)      => F.map2(f(domain), f(body))(Capture(_, _))
    case Record(name, typ, options) => F.map(f(typ))(Record(name, _, options))

  case Universe(options: TypeOptions) extends Lang[Nothing]

  case Record(name: RecordKey, typ: R, options: TypeOptions)
  case Extend(base: R, deps: R)
  case Function(domain: R, body: R)
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

  case Str(value: String) extends Lang[Nothing]
  case Float(value: Double) extends Lang[Nothing]
  case Integer(value: BigInt) extends Lang[Nothing]
  case Bool(value: Boolean) extends Lang[Nothing]

object Lang:
  extension [G[+x] >: Lang[x]](lang: Lang[Fix[G]]) def fix: Fix[G] = Fix(lang)

  given Conversion[String, Fix[Lang]]    = Str(_)
  given Conversion[scala.Int, Fix[Lang]] = Integer(_)
  given Conversion[Boolean, Fix[Lang]]   = Bool(_)

  object get extends Dynamic:
    def apply(key: RecordKey) = GetKey(key, 0)

    def unapply(key: GetKey): Option[RecordKey] = Option.when(key.up == 0)(key.key)

    def selectDynamic(name: String) = GetKey(name, 0)

  def set[G[+r] >: Lang[r]](key: RecordKey, t: Fix[G]): Fix[G] = Set(key, t).fix

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

  object recT extends Dynamic:
    def applyDynamicNamed[G[+r] >: Lang[r]](name: "apply")(assocs: (String, Fix[G])*): Fix[G] =
      assocs
        .map((name, t) => Record(name, t, TypeOptions()).fix)
        .reduceOption(Extend(_, _).fix)
        .getOrElse(Unit)

  class Call[G[+r] >: Lang[r]](term: Fix[G]) extends Dynamic:
    def applyDynamicNamed[H[+r] >: G[r]](name: "apply")(assocs: (String, Fix[H])*): Fix[H] =
      term.apply(rec.applyDynamicNamed("apply")(assocs*))

  extension [G[+r] >: Lang[r]](term: Fix[G])
    infix def |>[A, H[+r] >: G[r]](next: Fix[H]): Fix[H] = Lang.AndThen(term, next).fix

    def andThen[A, H[+r] >: G[r]](next: Fix[H]): Fix[H] = Lang.AndThen(term, next).fix

    def apply[H[+r] >: G[r]](args: Fix[H]): Fix[H] = rec(term, args) |> Apply

    def merge[H[+r] >: G[r]](ext: Fix[H]): Fix[H] = Merge(term, ext).fix

    def call: Call[G] = Call(term)

  given Traverse[Lang] = SimpleTraversing.traverseInstance[Lang]

type LangVal = Fix[Lang]
