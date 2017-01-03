package parseback

object SimpleParserSpec extends ParsebackSpec {

  "parentheses parser" should {
    lazy val p: Parser[Int] = (
        "(" ~> p <~ ")" ^^ { (_, i) => i + 1 }
      | () ^^^ 0
    )

    "accept the empty string" in {
      p must parseOk("")(0)
    }

    "accept a pair of parens" in {
      p must parseOk("()")(1)
    }

    "reject an opening paren" in {
      pending
    }

    "reject a closing paren" in {
      pending
    }

    "accept arbitrary nesting" in {   // TODO scalacheck
      p must parseOk("(((())))")(4)
    }

    "reject arbitrarily unbalanced nesting" in {
      pending
    }
  }

  "left-recursive repeater parser" should {
    lazy val p: Parser[Int] = (
        p <~ "a" ^^ { (_, i) => i + 1 }
      | "a" ^^^ 1
    )

    // TODO scalacheck
    "accept any number of repeating a's" in {
      p must parseOk("a")(1)
      p must parseOk("aaaa")(4)
      p must parseOk("aaaaaaaaaaaaaa")(14)
    }
  }

  "right-recursive repeater parser" should {
    lazy val p: Parser[Int] = (
        "a" ~> p ^^ { (_, i) => i + 1 }
      | "a" ^^^ 1
    )

    // TODO scalacheck
    "accept any number of repeating a's" in {
      p must parseOk("a")(1)
      p must parseOk("aaaa")(4)
      p must parseOk("aaaaaaaaaaaaaa")(14)
    }
  }
}
