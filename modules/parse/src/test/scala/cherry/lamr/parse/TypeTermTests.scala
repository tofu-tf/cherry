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
