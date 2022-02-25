package cherry.lamr.parse.term

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.*
import cherry.fix.Fix
import cherry.lamr.{Lang, RecordKey}
import cherry.lamr.norm.Term
import cherry.lamr.parse.basic.{*, given}
import cherry.lamr.parse.types.{arrow, typeTerm}
import tofu.syntax.monadic.*
import cats.parse.Parser
import cats.parse.Parser.char
import cherry.utils.collections

val application = smallTerm.repSep(whitespace).map(_.reduce(_.apply(_)))

private def makeLongArrow(t: Term, terms: NonEmptyList[(Term, Term)]): Term =
  val (init, last) = collections.swapReverse(t, terms.toList)
  init.foldLeft(last) { case (res, (domain, effect)) => Lang.Function(domain, effect, res).fix }

val longArrow = (application ~ (arrow ~ application).rep).map(makeLongArrow)

val chain = application.repSep(char(';') *> whitespace).map(_.reduce(_ |> _))

val theTerm: Parser[Term] = chain

val source: Parser[Term] = term.spaced <* end
