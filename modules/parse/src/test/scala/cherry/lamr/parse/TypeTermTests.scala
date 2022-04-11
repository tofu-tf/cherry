package cherry.lamr.parse

import cherry.lamr.{BuiltinType => BT, Lang, Term}

class TypeTermTests extends munit.FunSuite with ParseTesting:
  import Lang.{recT, get, rec, Builtin}
  test("unit") {
    "{}" shouldParse BT.Any
  }

  test("simple record") {
    "{a: Z, b: XX}" shouldParse recT(a = get.Z, b = get.XX)
  }

  test("complex record") {
    "{a: Z, b: {z: AA, v: {}}}" shouldParse recT(a = get.Z, b = recT(z = get.AA, v = BT.Any))
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

  test("lambda") {
    "{x: $int} => x" shouldParse recT(x = BT.Integer).lam(get.x)
  }

  test("lambda to arrow type") {
    "{A: $type} => A -> A" shouldParse recT(A = Lang.U).lam(get.A --> get.A)
  }

  test("lambda to effect arrow type") {
    "{A: $type, eff: $type} => A -[eff]> A" shouldParse recT(A = Lang.U, eff = Lang.U).lam(get.A -- get.eff --> get.A)
  }

  test("long lambda") {
    "{A: $type} => {x: A} => (z = x)" shouldParse recT(A = Lang.U).lam(recT(x = get.A).lam(rec(z = get.x)))
  }
