package cherry.lamr.norm

import cherry.utils.Act

class UmamiNormalizer(library: Library) extends Normalizer:
  def bigStep(term: PartialTerm, context: PartialTerm): Process[PartialTerm] =
    Act.Error(Error(Cause.Abort("ohhhh")))
end UmamiNormalizer
