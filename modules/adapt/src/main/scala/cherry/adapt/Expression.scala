package cherry.adapt

import cherry.fix.Fix.Fix
import cherry.lamr.Lang
import cherry.lamr.norm.{Library, NormValue, Normalizer, Term}

class Normalized(value: NormValue)

class Expression(expr: Term):
  def unpack: Lang[Expression] =
    expr.unpack match
      case l: Lang[Term]     => l.map(Expression(_))


