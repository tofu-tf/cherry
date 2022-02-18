package cherry.adapt

import cats.parse.Parser
import cherry.fix.Fix.Fix
import cherry.lamr.Lang
import cherry.lamr.norm.{Library, NormValue, Normalizer, Term}
import cherry.lamr.parse

class Expression(val expr: Term):
  def unpack: Lang[Expression] =
    expr.unpack match
      case l: Lang[Term] => l.map(Expression(_))

object Parsing:
  val sourceTerm: Parser[Term] = parse.term.source

  val sourceExpression: Parser[Expression] = sourceTerm.map(Expression(_))
