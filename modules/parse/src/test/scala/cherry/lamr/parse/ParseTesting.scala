package cherry.lamr.parse

import cherry.lamr.Lang
import cherry.lamr.parse.term.source
import cherry.lamr.norm.Term

trait ParseTesting extends munit.Assertions:
  extension (exp: String)
    infix def shouldParse(lang: Term) =
      assertEquals(source.parse(exp), Right(("", lang)))
