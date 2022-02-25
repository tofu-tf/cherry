package cherry.lamr.parse

import cherry.lamr.{BuiltinType, Lang}

class TermTests extends munit.FunSuite with ParseTesting:
  import Lang.{get, rec, Unit, Bool, Builtin}

  test("unit") {
    "()" shouldParse Unit
  }

  test("singleton tuple") {
    "(1, )" shouldParse rec(1)
  }

  test("integer tuple") {
    "(1, 2 ,3)" shouldParse rec(1, 2, 3)
  }

  test("record") {
    "(a = 1, b = 2, c = 3)" shouldParse rec(a = 1, b = 2, c = 3)
  }

  test("sample application") {
    "x z" shouldParse get("x").apply(get("z"))
  }

  test("complex application") {
    "x (a, b)" shouldParse get.x.apply(rec(get.a, get.b))
  }

  test("even more complex application") {
    "x y (z = (), y = x)" shouldParse get.x.apply(get.y).apply(rec(z = Unit, y = get.x))
  }

  test("very complex application") {
    "foo ( 3, 4 , 5 ) (a = 2, b = 3)" shouldParse get.foo.apply(rec(3, 4, 5)).apply(rec(a = 2, b = 3))
  }

  test("chaining") {
    "x ; y;z; w" shouldParse get.x.andThen(get.y).andThen(get.z).andThen(get.w)
  }

  test("chaining applications") {
    "x a; y a b; x y; z" shouldParse
      get.x
        .apply(get.a)
        .andThen(get.y.apply(get.a).apply(get.b))
        .andThen(get.x.apply(get.y))
        .andThen(get.z)
  }

  test("paren application") {
    "a (b c)" shouldParse get.a.apply(get.b.apply(get.c))
  }

  test("paren chaining") {
    "a ; b (c ; d)" shouldParse get.a |> get.b.apply(get.c |> get.d)
  }

  test("builtin tests") {
    "a $true" shouldParse get.a.apply(Bool(true))
    "($false, $str, $float, $int)" shouldParse rec(
      Bool(false),
      Builtin(BuiltinType.Str),
      Builtin(BuiltinType.Float),
      Builtin(BuiltinType.Integer)
    )
  }

end TermTests
