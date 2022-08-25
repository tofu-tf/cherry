package cherry.lamr.norm

import cherry.utils.Raising
import cherry.utils.Act

final case class NormState(context: NormValue, state: State = State(), term: Option[Term] = None):
  def context_=(context: NormValue): NormState = copy(context = context)

  def error(cause: => Cause) =
    state.errors :+= Error(cause, context, term)

object NormState:
  given Raising[NormState, Cause] with
    extension (ns: NormState) def raise(e: => Cause) = ns.error(e)

  extension [A](proc: Act[NormState, A])
    def locallyMergeContext(other: NormValue): Act[NormState, A] =
      for
        context <- Act.read((_: NormState).context)
        extCtx  <- context.merge(other)
        res     <- proc.locally(_.context = extCtx)
      yield res
