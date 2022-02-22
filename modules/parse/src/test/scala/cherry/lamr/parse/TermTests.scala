package cherry.lamr.parse

import cherry.fix.Fix
import cherry.lamr.parse.term.source
import cherry.lamr.{BuiltinType, Lang}

class TermTests extends munit.FunSuite {
  extension (exp: String)
    infix def shouldParse(lang: Fix[Lang]) =
      assertEquals(source.parse(exp), Right(("", lang)))

  test("unit") {
    "()" shouldParse Lang.Unit
  }

  test("singleton tuple") {
    "(1, )" shouldParse Lang.rec(1)
  }

  test("integer tuple") {
    "(1, 2 ,3)" shouldParse Lang.rec(1, 2, 3)
  }

  test("record") {
    "(a = 1, b = 2, c = 3)" shouldParse Lang.rec(a = 1, b = 2, c = 3)
  }

  test("sample application") {
    "x z" shouldParse Lang.get("x").apply(Lang.get("z"))
  }

  test("complex application") {
    "x (a, b)" shouldParse Lang.get.x.apply(Lang.rec(Lang.get.a, Lang.get.b))
  }

  test("even more complex application") {
    "x y (z = (), y = x)" shouldParse Lang.get.x.apply(Lang.get.y).apply(Lang.rec(z = Lang.Unit, y = Lang.get.x))
  }

  test("very complex application") {
    "foo ( 3, 4 , 5 ) (a = 2, b = 3)" shouldParse Lang.get.foo.apply(Lang.rec(3, 4, 5)).apply(Lang.rec(a = 2, b = 3))
  }

  test("chaining") {
    "x ; y;z; w" shouldParse Lang.get.x.andThen(Lang.get.y).andThen(Lang.get.z).andThen(Lang.get.w)
  }

  test("chaining applications") {
    "x a; y a b; x y; z" shouldParse
      Lang.get.x
        .apply(Lang.get.a)
        .andThen(Lang.get.y.apply(Lang.get.a).apply(Lang.get.b))
        .andThen(Lang.get.x.apply(Lang.get.y))
        .andThen(Lang.get.z)
  }

  test("paren application") {
    "a (b c)" shouldParse Lang.get.a.apply(Lang.get.b.apply(Lang.get.c))
  }

  test("paren chaining") {
    "a ; b (c ; d)" shouldParse Lang.get.a |> Lang.get.b.apply(Lang.get.c |> Lang.get.d)
  }

  test("builtin tests") {
    "a $true" shouldParse Lang.get.a.apply(Lang.Bool(true))
    "($false, $str, $float, $int)" shouldParse Lang.rec(
      Lang.Bool(false),
      Lang.Builtin(BuiltinType.Str),
      Lang.Builtin(BuiltinType.Float),
      Lang.Builtin(BuiltinType.Integer)
    )
  }

}
