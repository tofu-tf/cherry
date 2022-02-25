package cherry.utils

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object collections:
  def swapReverse[A, B](head: A, rest: List[(B, A)], acc: List[(A, B)] = Nil): (List[(A, B)], A) =
    rest match
      case (b, a) :: tail => swapReverse(a, tail, (head, b) +: acc)
      case _              => (acc, head)
