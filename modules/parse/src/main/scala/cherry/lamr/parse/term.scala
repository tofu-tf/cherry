package cherry.lamr.parse

package term

import cats.parse.Parser
import Parser._
import tofu.syntax.monadic._
import cherry.lamr.Lang
import cherry.fix.Fix
import cherry.lamr.RecordKey

import basic._

val term = defer(theTerm)

val symbolKey = (identifier.map(RecordKey.Symbol(_)) <* whitespace).backtrack

val separator = char(',').orElse(char(';')).spaced

val listSyntax = term.repSep0(separator).map {
  _.iterator.zipWithIndex.map { (t, i) => Lang.Set(RecordKey.Index(i), t).fix }
    .foldLeft[Fix[Lang]](Lang.Unit)(Lang.Extend(_, _).fix)
}

val symbolTerm = (symbolKey ~ ((char('=') *> whitespace *> term).?)).map {
  case (key, None)    => Lang.Get(key, 0)
  case (key, Some(t)) => Lang.Set(key, t).fix
}

val listTerm = char('[') *> listSyntax <* char(']')

val recordSyntax = term.repSep(separator).map(_.foldLeft[Fix[Lang]](Lang.Unit)(Lang.Extend(_, _).fix))

val recordTerm = char('(') *> recordSyntax <* char(')')

val integerTerm = integer.map(Lang.Integer(_))

val arguments = char('(') *> recordSyntax.orElse(term) <* char(')')

val fixedTerm = oneOf(List(integerTerm, recordTerm, listTerm, symbolTerm))

val application = fixedTerm.repSep(whitespace).map(_.reduce(_.apply(_)))

val theTerm: Parser[Fix[Lang]] = application.spaced

val termParser = term *> end

@main def testa() =
  val strings = Vector(
    """z = []""",
    """ ( x = 1 , y = 2,  z = []) """,
    "z z",
    """foo [ 3, 4 , 5 ] (a = 2, b = 3)""",
  )

  val s1 = "(1, 2, 3)"

  Lang.rec(x = Lang.Unit, y = Lang.Integer(2))
  for string <- strings do println(term.parse(string))
