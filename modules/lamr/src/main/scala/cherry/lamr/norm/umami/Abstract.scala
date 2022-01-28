package cherry.lamr.norm
package umami

import cherry.fix.Fix
import cherry.lamr.Lang
import cherry.utils.Act

class UmamiAbstract(term: PartialTerm, val posistion: Option[Position] = None) extends NormValue:
  def toPartial = term

  def apply(arg: PartialTerm) =
    UmamiAbstract(Fix(Lang.AndThen(term, arg)), posistion)

  
