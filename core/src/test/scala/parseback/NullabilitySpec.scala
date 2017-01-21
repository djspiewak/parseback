package parseback

object NullabilitySpec extends ParsebackSpec {

  "non-recursive nullability" should {
    "handle sequence by conjunction" in {
      ("a" ~ "b").isNullable must beFalse
      (() ~ "b").isNullable must beFalse
      ("a" ~ (())).isNullable must beFalse
      (() ~ (())).isNullable must beTrue
    }

    "handle union by disjunction" in {
      ("a" | "b").isNullable must beFalse
      (() | "b").isNullable must beTrue
      ("a" | (())).isNullable must beTrue
      (() | (())).isNullable must beTrue
    }

    "delegate apply" in {
      ("a" ^^ { (_, _) => () }).isNullable must beFalse
      (() ^^ { (_, _) => () }).isNullable must beTrue
    }

    "define literal" in {
      "a".isNullable must beFalse
    }

    "define epsilon" in {
      ().isNullable must beTrue
    }

    "define failure" in {
      Parser.Failure(Nil).isNullable must beTrue
    }
  }

  "recursive nullability" should {
    "reduce for false with left-recursion" in {
      lazy val p: Parser[Any] = (
          p ~ "a"
        | "a"
      )

      p.isNullable must beFalse
    }

    "reduce for true with left-recursion" in {
      lazy val p: Parser[Any] = (
          p ~ "a"
        | (())
      )

      p.isNullable must beTrue
    }

    "reduce for false with right-recursion" in {
      lazy val p: Parser[Any] = (
          "a" ~ p
        | "a"
      )

      p.isNullable must beFalse
    }

    "reduce for true with right-recursion" in {
      lazy val p: Parser[Any] = (
          "a" ~ p
        | (())
      )

      p.isNullable must beTrue
    }

    "solve for true nullability with vacuous recursion" in {
      lazy val p: Parser[Any] = () | p

      p.isNullable must beTrue
    }

    "solve for false nullability with vacuous recursion" in {
      lazy val p: Parser[Any] = "a" | p

      p.isNullable must beFalse
    }

    "produce error with trivially vacuous union" in {
      lazy val p: Parser[Any] = p | p

      p.isNullable must throwAn[Exception]
    }
  }
}
