package cherry.fix

class TestTraverse extends munit.FunSuite {
  test("test wraps") {
    assertEquals(Wraps(1).map(_.toDouble), Wraps(1.0))
    assertEquals(Wraps(1).traverse(i => ("yy", s"$i!")), ("yy", Wraps("1!")))
  }

  test("test wraps2") {
    assertEquals(Wraps2(1, 2, "3", 4).map(_.toDouble), Wraps2(1, 2.0, "3", 4.0))
    assertEquals(Wraps2(1, 2, "3", 4).traverse(i => ("yy", s"$i!")), ("yyyy", Wraps2(1, "2!", "3", "4!")))
  }

  test("test wraps3") {
    assertEquals((Wraps3.Empty: Wraps3[Int]).map(_.toDouble), (Wraps3.Empty: Wraps3[Double]))
    assertEquals(Wraps3.Single(1).map(_.toDouble), Wraps3.Single(1.0))
    assertEquals(Wraps3.PairAndInt(1, 2, 3).map(_.toDouble), Wraps3.PairAndInt(1.0, 2, 3.0))

    assertEquals((Wraps3.Empty: Wraps3[Int]).traverse(i => ("yy", s"$i!")), ("", Wraps3.Empty: Wraps3[String]))
    assertEquals(Wraps3.Single(1).traverse(i => ("yy", s"$i!")), ("yy", Wraps3.Single("1!")))
    assertEquals(Wraps3.PairAndInt(1, 2, 3).traverse(i => ("yy", s"$i!")), ("yyyy", Wraps3.PairAndInt("1!", 2, "3!")))
  }
}

case class Wraps[+A](x: A) derives Traverse
case class Wraps2[+A](i: Int, x: A, s: String, y: A) derives Traverse

enum Wraps3[+A] derives Traverse:
  case Empty
  case Single(x: A)
  case PairAndInt(x: A, u: Int, y: A)
