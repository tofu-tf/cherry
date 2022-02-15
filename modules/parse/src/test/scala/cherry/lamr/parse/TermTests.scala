package cherry.lamr.parse

import cherry.fix.Fix
import cherry.lamr.Lang
import cherry.lamr.parse.term.source

class TermTests extends munit.FunSuite {
  extension (exp: String)
    infix def shouldParse(lang: Fix[Lang]) =
      assertEquals(source.parse(exp), Right(("", lang)))

  test("unit") {
    "[]" shouldParse Lang.Unit
  }

  test("singleton tuple") {
    "[1]" shouldParse Lang.rec(1)
  }

  test("integer tuple") {
    "[1, 2 ,3]" shouldParse Lang.rec(1, 2, 3)
  }

  test("record") {
    "(a = 1, b = 2, c = 3)" shouldParse Lang.rec(a = 1, b = 2, c = 3)
  }

  test("sample application") {
    "x z" shouldParse Lang.get("x").apply(Lang.get("z"))
  }

  test("complex application") {
    "x [a, b]" shouldParse Lang.get.x.apply(Lang.rec(Lang.get.a, Lang.get.b))
  }

  test("very complex application") {
    "foo [ 3, 4 , 5 ] (a = 2, b = 3)" shouldParse Lang.get.foo.apply(Lang.rec(3, 4, 5)).apply(Lang.rec(a = 2, b = 3))
  }

  test("complex chaning") {
    "x a ; z z ; z z" shouldParse Lang.get.x
      .apply(Lang.get.a)
      .andThen(Lang.get.z.apply(Lang.get.z))
      .andThen(Lang.get.z.apply(Lang.get.z))
  }

}
