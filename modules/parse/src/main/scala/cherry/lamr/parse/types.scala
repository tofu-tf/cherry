package cherry.lamr.parse

package types

import cats.parse.Parser
import Parser._
import tofu.syntax.monadic._
import cherry.lamr.Lang
import cherry.fix.Fix
import cherry.lamr.RecordKey
import cherry.lamr.BuiltinType
import basic._
import cherry.lamr.TypeOptions
import term._

val typeSeparator = char(':').as(TypeOptions())

val builtinType = Vector(
  "str"  -> BuiltinType.Str,
  "int"  -> BuiltinType.Integer,
  "bool" -> BuiltinType.Bool
).map((name, bt) => string(name) as bt).reduce(_.orElse(_))

val builtin = char('$') *> builtinType.map(Lang.Builtin(_))

val recordFieldtype = (symbolKey, typeSeparator.spaced, term).tupled

val recordType = (char('{') *> recordFieldtype.repSep(char(',').spaced).spaced <* char('}')).map(ts =>
  ts.map((name, _, t) => Lang.Record(name, t).fix).reduce(Lang.Extend(_, _).fix)
)

val typeTerm = Parser.oneOf(List(builtin, recordType))
