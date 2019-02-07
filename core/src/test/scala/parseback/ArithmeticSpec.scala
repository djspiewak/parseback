/*
 * Copyright 2018 Daniel Spiewak
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

object ArithmeticSpec extends ParsebackSpec {
  import ParseError._

  "expression evaluator" should {
    val whitespace = """\s+""".r
    val numR = """\d+""".r
    implicit val lexer = LexerHelper.lexer(Option(whitespace), Set.empty, Set("+", "-", "*", "/", "(", ")"), Set(numR))

    lazy val expr: Parser[Int] = (
        expr ~ "+" ~ term ^^ { (_, e, _, t) => e + t }
      | expr ~ "-" ~ term ^^ { (_, e, _, t) => e - t }
      | term
    )

    lazy val term: Parser[Int] = (
        term ~ "*" ~ factor ^^ { (_, e, _, f) => e * f }
      | term ~ "/" ~ factor ^^ { (_, e, _, f) => e / f }
      | factor
    )

    lazy val factor: Parser[Int] = (
        "(" ~> expr <~ ")"
      | "-" ~ expr  ^^ { (_, _, e) => -e }
      | numR ^^ { (_, str) => str.toInt }
    )

    "read and eval `1 + 2`" in {
      expr must parseOk("1 + 2")(3)
    }

    "correctly handle precedence" in {
      expr must parseOk("3 + 4 * 5")(23)
      expr must parseOk("(3 + 4) * 5")(35)
    }

    "reject partial expressions" in {
      expr must failToParse("3 + *")(UnexpectedCharacter(Line(Token("3", "+", "*"), 0, 2), Set("""\d+""", "-", "(")))
    }
  }
}
