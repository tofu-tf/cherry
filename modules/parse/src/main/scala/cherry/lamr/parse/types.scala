package cherry.lamr.parse

package types

import cats.parse.Parser
import Parser._
import tofu.syntax.monadic._
import cherry.lamr.{Lang, Term}
import cherry.fix.Fix
import cherry.lamr.RecordKey
import cherry.lamr.BuiltinType
import cherry.lamr.TypeOptions
import term._
import basic._

lazy val typeSeparator = char(':').as(TypeOptions())

lazy val recordFieldtype = (symbolKey, typeSeparator.spaced, term).tupled

lazy val recordType = (char('{') *> recordFieldtype.repSep0(char(',').spaced).spaced0 <* char('}')).map(ts =>
  ts.iterator
    .map((name, opts, t) => Lang.Record(name, t, opts).fix)
    .reduceOption(Lang.Extend(_, _).fix)
    .getOrElse(Lang.Builtin(BuiltinType.Any))
)

val typeArrow = Parser.defer(theTypeArrow)

lazy val typeTerm = Parser.oneOf(List(builtin, recordType))

lazy val effect                     = char('[') *> term <* char(']')
lazy val theTypeArrow: Parser[Term] = (char('-') *> effect.? <* char('>')).map(_.getOrElse(BuiltinType.Any))

val lambdaArrow = string("=>")
