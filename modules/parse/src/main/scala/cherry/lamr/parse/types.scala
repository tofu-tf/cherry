package cherry.lamr.parse

package types

import cats.parse.Parser
import Parser._
import tofu.syntax.monadic._
import cherry.lamr.Lang
import cherry.fix.Fix
import cherry.lamr.RecordKey
import cherry.lamr.BuiltinType
import cherry.lamr.TypeOptions
import term._
import basic._

val typeSeparator = char(':').as(TypeOptions())

val recordFieldtype = (symbolKey, typeSeparator.spaced, term).tupled

val recordType = (char('{') *> recordFieldtype.repSep0(char(',').spaced).spaced0 <* char('}')).map(ts =>
  ts.iterator
    .map((name, opts, t) => Lang.Record(name, t, opts).fix)
    .reduceOption(Lang.Extend(_, _).fix)
    .getOrElse(Lang.Builtin(BuiltinType.Any))
)

val typeTerm = Parser.oneOf(List(builtin, recordType))
