package cherry.lamr.parse.simple

import cats.parse.Parser
import Parser._
import tofu.syntax.monadic._
import cherry.lamr.Lang
import cherry.fix.Fix
import cherry.lamr.RecordKey

val lowerLetter = charIn('a' to 'z')
val upperLetter = charIn('A' to 'Z')
val underscore  = char('_')
val digit       = charIn('0' to '9')
val nonZero     = charIn('1' to '9')
val zero        = char('0')

val letter        = lowerLetter orElse upperLetter orElse underscore
val letterOrDigit = letter orElse digit

val identifier = (letter *> letterOrDigit.rep0).string

val positiveInt = (nonZero *> digit.rep0).string.map(BigInt(_))
val negativeInt = (char('-') *> positiveInt).map(-_)
val zeroInt     = char('0') as BigInt(0)

val integer = zeroInt orElse positiveInt orElse negativeInt

val whitespace = charIn(' ', '\t', '\n').rep0.void

extension [A](p: Parser[A]) def spaced: Parser[A] = p.surroundedBy(whitespace)

val term = defer(theTerm)

val symbolKey = (identifier.map(RecordKey.Symbol(_)) <* whitespace).backtrack

val separator = char(',').orElse(char(';')).spaced

val listSyntax = term.repSep0(separator).map {
  _.iterator.zipWithIndex.map { (t, i) => Lang.Set(RecordKey.Index(i), t).fix }
    .foldLeft[Fix[Lang]](Lang.Unit)(Lang.AndThen(_, _).fix)
}

val symbolTerm = (symbolKey ~ ((char('=') *> whitespace *> term).?)).map {
  case (key, None)    => Lang.Get(key)
  case (key, Some(t)) => Lang.Set(key, t).fix
}

val listTerm = char('[') *> listSyntax <* char(']')

val recordSyntax = term.repSep(separator).map(_.reduce(Lang.AndThen(_, _).fix))

val recordTerm = char('(') *> recordSyntax <* char(')')

val integerTerm = integer.map(Lang.Int(_))

val arguments = char('(') *> recordSyntax.orElse(term) <* char(')')

val fixedTerm = oneOf(List(integerTerm, recordTerm, listTerm, symbolTerm))

val application = fixedTerm.repSep(whitespace).map(_.reduce(_(_)))

val theTerm: Parser[Fix[Lang]] = application.spaced

@main def testa() =
  val strings = Vector(
    """z = []""",
    """ ( x = 1 , y = 2,  z = []) """,
    "z z",
    """foo [ 3, 4 , 5 ] (a = 2, b = 3)""",
  )

  val s1 = "(1, 2, 3)"

  Lang.rec(x = Lang.Unit, y = Lang.Int(2))
  for string <- strings do println(term.parse(string))
