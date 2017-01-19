/*
 * Copyright 2017 Daniel Spiewak
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

  sequential    // TODO !!!

  "expression evaluator" should {
    implicit val W = Whitespace("""\s+"""r)

    lazy val expr: Parser[Int] = (
        expr ~ "+" ~ term ^^ { (_, e, _, t) => e + t }
      | expr ~ "-" ~ term ^^ { (_, e, _, t) => e - t }
      | term
    )

    lazy val term: Parser[Int] = (
        expr ~ "*" ~ factor ^^ { (_, e, _, f) => e * f }
      | expr ~ "/" ~ factor ^^ { (_, e, _, f) => e / f }
      | factor
    )

    lazy val factor: Parser[Int] = (
        "(" ~> expr <~ ")"
      | "-" ~ expr  ^^ { (_, _, e) => -e }
      | """\d+""".r ^^ { (_, str) => str.toInt }
    )

    "read and eval `1 + 2`" in {
      expr must parseOk("1 + 2")(3)
    }

    "correctly handle precedence" in {
      expr must parseOk("3 + 4 * 5")(23)
      expr must parseOk("(3 + 4) * 5")(35)
    }.pendingUntilFixed

    "reject partial expressions" in {
      expr must failToParse("3 + *")(UnexpectedCharacter(Line("3 + *", 0, 4), Set("""\d+""", "-", "(")))
    }
  }
}
