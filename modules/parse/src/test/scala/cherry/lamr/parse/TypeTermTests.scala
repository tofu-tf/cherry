package cherry.lamr.parse

import cherry.lamr.{BuiltinType, Lang, Term}

class TypeTermTests extends munit.FunSuite with ParseTesting:
  import Lang.{recT, get, Builtin}
  test("unit") {
    "{}" shouldParse Lang.Builtin(BuiltinType.Any)
  }

  test("simple record") {
    "{a: Z, b: XX}" shouldParse recT(a = get.Z, b = get.XX)
  }

  test("complex record") {
    "{a: Z, b: {z: AA, v: {}}}" shouldParse recT(a = get.Z, b = recT(z = get.AA, v = BuiltinType.Any))
  }

  test("arrow type") {
    "a -> b" shouldParse (get.a --> get.b)
  }

  test("long arrow type") {
    "a -> b -> c -> d" shouldParse get.a --> (get.b --> (get.c --> get.d))
  }

  test("parensed arrow type") {
    "(a -> b) -> (c -> d)" shouldParse ((get.a --> get.b) --> (get.c --> get.d))
  }

  test("effectful arrow type") {
    "a -[e]> b" shouldParse (get.a -- get.e --> get.b)
  }

  test("long arrow effectful type") {
    "a -[e x]> b -[ee y]> c -[ez y u]> d" shouldParse
      get.a -- get.e.apply(get.x) --> (
        get.b -- get.ee.apply(get.y) --> (
          get.c -- get.ez.apply(get.y).apply(get.u) --> get.d
        )
      )
  }
