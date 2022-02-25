package cherry.lamr.parse.term

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.*
import cherry.fix.Fix
import cherry.lamr.{Lang, RecordKey}
import cherry.lamr.norm.Term
import cherry.lamr.parse.basic.{*, given}
import cherry.lamr.parse.types.typeTerm
import tofu.syntax.monadic.*

import cats.parse.Parser
import cats.parse.Parser.char

val application = smallTerm.repSep(whitespace).map(_.reduce(_.apply(_)))

val chain = application.repSep(char(';') *> whitespace).map(_.reduce(_ |> _))

val theTerm: Parser[Term] = chain

val source: Parser[Term] = term.spaced <* end
