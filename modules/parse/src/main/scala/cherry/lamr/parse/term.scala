package cherry.lamr.parse

package term

import cats.parse.Parser
import Parser._
import tofu.syntax.monadic._
import cherry.lamr.Lang
import cherry.fix.Fix
import cherry.lamr.RecordKey

import basic._
import basic.given
import types.typeTerm

val term: Parser[Fix[Lang]] = defer(theTerm)

val separator = char(',')

val listSyntax = term.spaced
  .repSep0(separator)
  .map(_.iterator.zipWithIndex.map { (t, i) => Lang.set(RecordKey.Index(i), t) })
  .map(mergeAll)

val symbolTerm: Parser[Fix[Lang]] =
  ((symbolKey <* whitespace) ~ (char('=') *> whitespace *> term).?).map {
    case (key, None)    => Lang.get(key)
    case (key, Some(t)) => Lang.set(key, t)
  }

def mergeAll(terms: IterableOnce[Fix[Lang]]): Fix[Lang] =
  terms.iterator.reduceOption(Lang.Merge(_, _).fix).getOrElse(Lang.Unit)

val assignment = (symbolKey, char('=').spaced *> term).mapN((key, t) => Lang.set(key, t))

val recordSyntax = assignment.spaced.repSep0(separator).map(mergeAll)

val listTerm = char('[') *> listSyntax <* char(']')

val recordTerm = char('(') *> recordSyntax <* char(')')

val integerTerm = integer.map(Lang.Integer(_))

val arguments = char('(') *> recordSyntax.orElse(term) <* char(')')

val smallTerm = oneOf(List(integerTerm, recordTerm, listTerm, symbolTerm, typeTerm))

val application = smallTerm.repSep(whitespace).map(_.reduce(_.apply(_)))

val chain = application.repSep(char(';') *> whitespace).map(_.reduce(_ |> _))

val theTerm: Parser[Fix[Lang]] = chain

val source: Parser[Fix[Lang]] = term.spaced <* end

@main def testa() =

  val strings = Vector(
    """z = []""",
    """ ( x = 1 , y = 2,  z = []) """,
    "z",
    "z z",
    """foo [ 3, 4 , 5 ] (a = 2, b = 3)""",
    "x a ; z z ; z z"
  )

  val s1 = "(1, 2, 3)"

  Lang.rec(x = Lang.Unit, y = Lang.Integer(2))
  for string <- strings do println(source.parse(string))
