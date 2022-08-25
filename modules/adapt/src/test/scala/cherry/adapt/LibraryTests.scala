package cherry.adapt

import cherry.adapt.Parsing.sourceExpression
import cherry.lamr.norm.BuiltinLibrary
import cherry.lamr.norm.umami.{StringValue, BooleanValue, FloatValue, IntegerValue, UmamiNormalizer}

class LibraryTests extends munit.FunSuite:

  val n = new UmamiNormalizer(BuiltinLibrary)

  test("ints plus") {
    val t = "ints ; plus [1, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(3)))
  }

  test("ints minus") {
    val t = "ints ; minus [1, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(-1)))
  }

  test("ints div") {
    val t = "ints ; div [4, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(2)))
  }

  test("ints mul") {
    val t = "ints ; mul [4, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(8)))
  }

  test("ints xor") {
    val t = "ints ; xor [4, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(6)))
  }

  test("ints bitand") {
    val t = "ints ; bitand [4, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(0)))
  }

  test("ints bitor") {
    val t = "ints ; bitor [4, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(6)))
  }

  test("ints gcd") {
    val t = "ints ; gcd [4, 2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(IntegerValue(2)))
  }

  test("floats plus") {
    val t = "floats ; plus [1.5, 2.5]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(FloatValue(4.0)))
  }

  test("floats minus") {
    val t = "floats ; minus [1.1, 2.2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(FloatValue(-1.1)))
  }

  test("floats div") {
    val t = "floats ; div [4.4, 2.2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(FloatValue(2.0)))
  }

  test("floats mul") {
    val t = "floats ; mul [4.4, 2.2]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(FloatValue(4.4 * 2.2)))
  }

  test("bools xor") {
    val t = "bools ; xor [true, false]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(BooleanValue(true)))
  }

  test("bools bitand") {
    val t = "bools ; bitand [true, false]"
    val x = sourceExpression.parse(t)
    assertEquals(x.toOption.get._2.normalize(n), Right(BooleanValue(false)))
  }

  test("bools bitor") {
    val t = "bools ; bitor [true, false]"
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
