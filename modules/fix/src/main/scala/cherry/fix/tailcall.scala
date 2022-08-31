package cherry.fix

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

def delay[A](a: => A): TailRec[A] = TailCalls.tailcall(TailCalls.done(a))

export TailCalls.tailcall

extension [A](t: TailRec[A])
  def memoize: TailRec[A] =
    lazy val result = t.result
    delay(result)
