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

val term = defer(composeTerm)

val assignment = identifier.spaced ~ (char('=') *> term)

val separator = char(',').orElse(char(';')).spaced

val recordItem = assignment.eitherOr(term)

val listTerm = recordItem.repSep0(separator).map { ass =>
  ass.iterator
    .foldLeft((0, Vector.empty[(RecordKey, Fix[Lang])])) {
      case ((num, acc), Left(t))          => (num + 1, acc :+ (RecordKey.Index(num), t))
      case ((num, acc), Right(name -> t)) => (num, acc :+ (RecordKey.Symbol(name), t))
    }
    ._2
}

val record = (char('[') *> recordList <* char(']')).map { ass =>
  ass.iterator
    .map((key, term) => Lang.Set(key, term).fix)
    .reduceOption(Lang.AndThen(_, _).fix)
    .getOrElse(Lang.Unit)
}

val integerTerm = integer.map(Lang.Int(_))

val composeTerm: Parser[Fix[Lang]] = oneOf(List(integerTerm, record)).spaced

@main def testa() =
  val string = """ [ x = 1 , y = 2, 3, 4, [] , z = []] """
  println(term.parse(string))
