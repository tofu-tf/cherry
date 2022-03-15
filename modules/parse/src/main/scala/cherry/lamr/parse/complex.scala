package cherry.lamr.parse.term

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.*
import cherry.fix.Fix
import cherry.lamr.{Lang, RecordKey}
import cherry.lamr.norm.Term
import cherry.lamr.parse.basic.{*, given}
import cherry.lamr.parse.types.{typeArrow, typeTerm, lambdaArrow}
import tofu.syntax.monadic.*
import cats.parse.Parser
import cats.parse.Parser.char
import cherry.utils.collections

val application = smallTerm.repSep(whitespace).map(_.reduce(_.apply(_)))

private def makeLongArrow(t: Term, terms: List[(Term, Term)]): Term =
  val (init, last) = collections.swapReverse(t, terms)
  init.foldLeft(last) { case (res, (domain, effect)) => Lang.Function(domain, effect, res).fix }

val longArrow: Parser[Term] =
  ((application <* whitespace) ~ (typeArrow ~ (whitespace *> application)).rep0).map(makeLongArrow)

val longLambda = (longArrow <* whitespace).repSep(lambdaArrow <* whitespace).map(_.toList.reduceRight(Lang.Capture(_, _).fix))

val chain = longLambda.repSep(char(';') *> whitespace).map(_.reduce(_ |> _))

val theTerm: Parser[Term] = chain

val source: Parser[Term] = term.spaced <* end

@main def testc() =
  println(longArrow.parse("a -> b"))
