package cherry.lamr.norm
package umami

import cherry.utils.Act
import cherry.lamr.Lang

class UmamiNormalizer(library: Library) extends Normalizer:
  def bigStep(term: PartialTerm, context: NormValue): Process[NormValue] = term.unpack match
    case Lang.Unit       => Act.Pure(UnitValue)
    case Lang.Integer(v) => Act.Pure(IntegerValue(v))
