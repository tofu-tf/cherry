package cherry.adapt

import cherry.adapt.Parsing.sourceExpression
import cherry.lamr.norm.BuiltinLibrary
import cherry.lamr.norm.umami.{StringValue, BooleanValue, FloatValue, IntegerValue, UmamiNormalizer}

class LibraryTests extends munit.FunSuite:

  test("ints plus") {
    val n = new UmamiNormalizer(BuiltinLibrary)
    val t = "ints ; plus [1, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(3)))
  }

  test("float plus") {
    val n = new UmamiNormalizer(BuiltinLibrary)
    val t = "floats ; plus [1.5, 2.5]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(FloatValue(4.0)))
  }

  test("bools xor") {
    val n = new UmamiNormalizer(BuiltinLibrary)
    val t = "bools ; xor [true, false]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(BooleanValue(true)))
  }

  test("strs plus") {
    val n = new UmamiNormalizer(BuiltinLibrary)
    val t = """strs ; plus ["one", "two"]"""
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(StringValue("onetwo")))
  }

end LibraryTests
