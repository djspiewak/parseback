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
}
