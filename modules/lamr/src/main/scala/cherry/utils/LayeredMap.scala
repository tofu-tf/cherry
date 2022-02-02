package cherry.utils

import scala.collection.immutable.TreeMap
import cats.Traverse
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec

object LayeredMap:
  opaque type LM[K, +V] <: Iterable[Iterable[(K, V)]] = List[SortedMap[K, V]]

  extension [K, V](generations: LM[K, V])
    def get(gen: Int, key: K): Option[V] = generations.lift(gen).flatMap(_.get(key))

  given [K]: Traverse[[a] =>> LayeredMap[K, a]] = Traverse[List].compose[[a] =>> SortedMap[K, a]]

type LayeredMap[K, +V] = LayeredMap.LM[K, V]

@tailrec
def length(c: Char)(x: Int = c.toInt * c.toInt, len: Int = 0): Int =
  if (x == c.toInt) len else length(c)((x * c.toInt) % Char.MaxValue, len + 1)


@main def hello()     =
  for c <- 'a' to 'z' do
    println(c -> length(c)())

// def kriwda(s: String) =
//   (s.view.map(_.toInt).sum, s.foldLeft(0)(_ ^ _), s.foldLeft(1)((x, y) => (x * y) % Char.MaxValue))

