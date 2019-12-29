/*
 * Copyright 2019 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package parseback

class NullabilitySpec extends ParsebackSpec {

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

    "solve for false with trivially vacuous union" in {
      lazy val p: Parser[Any] = p | p

      p.isNullable must beFalse
    }

    "detect a nullable RHS past nested recursion" in {
      lazy val left: Parser[Any] = (
          left <~ "+"
        | right
      )

      lazy val right: Parser[Any] = (
          right <~ "*"
        | () ^^^ 1
      )

      left.isNullable must beTrue
    }

    "detect nullability in Michael's example" in {
      lazy val a: Parser[Any] = b ~ c
      lazy val b: Parser[Any] = c | () ^^^ ""
      lazy val c: Parser[Any] = b | "x"

      a.isNullable must beTrue
    }
  }
}
