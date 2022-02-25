package cherry.lamr
import cats.{Applicative, Eval, Traverse}
import cherry.fix.Fix
import cherry.lamr.Lang.{Capture, External, GetKey, Universe}
import cherry.utils.SimpleTraversing

import scala.annotation.targetName
import scala.language.dynamics

// concat : {I :~ Type, 0: List(I), 1: List(I)} -> List(I)
// concat (list1, list2)
// concat (I = int, list1, list2)
// concat(list1, list2)
// concat[int](list1, list2)
// {x: int, y: str} = Extend({x: int} , {y: str}) = {x: int} & {y: str}
// (x = 1, y = "lol") = Merge((x = 1), (y = "lol")) = (x = 1) .. (y = "lol")

// (x : Nat ** Vec x Int)
// (0 = 3) .. (0 = 4)    ..  (0 = 1) .. (0 = 2) = (0^3 = 3, 0^2 = 4, 0^1 = 1, 0 = 2)
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

enum Lang[+R] extends SimpleTraversing[Lang, R]:
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

  case Str(value: String) extends Lang[Nothing]
  case Float(value: Double) extends Lang[Nothing]
  case Integer(value: BigInt) extends Lang[Nothing]
  case Bool(value: Boolean) extends Lang[Nothing]

  def traverse[F[_], X](f: R => F[X])(using F: Applicative[F]): F[Lang[X]] = this match
    case u @ (
          _: Universe | _: Builtin | Id | Unit | Apply | _: Str | _: Float | _: Integer | _: Bool | _: External |
          _: GetKey
        ) =>
      F.pure(u)
    case Extend(base, deps)               => F.map2(f(base), f(deps))(Extend(_, _))
    case Function(domain, effect, result) => F.map3(f(domain), f(effect), f(result))(Function(_, _, _))
    case Set(key, term)                   => F.map(f(term))(Set(key, _))
    case Merge(base, deps)                => F.map2(f(base), f(deps))(Merge(_, _))
    case Narrow(term, typ)                => F.map2(f(term), f(typ))(Narrow(_, _))
    case AndThen(left, right)             => F.map2(f(left), f(right))(AndThen(_, _))
    case Capture(domain, body)            => F.map2(f(domain), f(body))(Capture(_, _))
    case Record(name, typ, options)       => F.map(f(typ))(Record(name, _, options))

type Term = Fix[Lang]

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

    def andThen(next: Term): Term = Lang.AndThen(term, next).fix

    def apply(args: Term): Term = rec(term, args) |> Apply

    def merge(ext: Term): Term = Merge(term, ext).fix

    def call: Call = Call(term)

  given Conversion[BuiltinType, Term] = Lang.Builtin(_)

  given Traverse[Lang] = SimpleTraversing.traverseInstance[Lang]

type LangVal = Fix[Lang]
