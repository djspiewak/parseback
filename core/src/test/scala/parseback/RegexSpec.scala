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

class RegexSpec extends ParsebackSpec {

  "regex parsing" should {
    "consume an integer literal" in {
      regex("""\d+""".r) must parseOk("42")("42")
    }

    "handle a simple arithmetic grammar" in {
      lazy val expr: Parser[Int] = (
          expr ~ "+" ~ expr ^^ { (_, a, _, b) => a + b }
        | expr ~ "-" ~ expr ^^ { (_, a, _, b) => a - b }
        | """\d+""".r ^^ { (_, str) => str.toInt }
      )

      expr must parseOk("1+2")(3)
    }

    "handle a simple arithmetic grammar with whitespace" in {
      implicit val W = Whitespace(() | """\s+""".r)

      lazy val expr: Parser[Int] = (
          expr ~ "+" ~ expr ^^ { (_, a, _, b) => a + b }
        | expr ~ "-" ~ expr ^^ { (_, a, _, b) => a - b }
        | """\d+""".r ^^ { (_, str) => str.toInt }
      )

      expr must parseOk("1 + 2")(3)
    }

    /*
     * note to self: the following test checks something important,
     * which is that a root parser (whitespace, in this case) can
     * be used at different points in the stream that are
     * coincidentally character-equal.  specifically, index 1 and
     * index 5 are character-equal, but require different derivations
     * from the """\s+""".r parser (the first to Literal(" "), the
     * second to Literal("   "))
     */
    "handle a simple arithmetic grammar with trailing whitespace" in {
      implicit val W = Whitespace(() | """\s+""".r)

      lazy val expr: Parser[Int] = (
          expr ~ "+" ~ expr ^^ { (_, a, _, b) => a + b }
        | expr ~ "-" ~ expr ^^ { (_, a, _, b) => a - b }
        | """\d+""".r ^^ { (_, str) => str.toInt }
      )

      expr must parseOk("1 + 2   ")(3)
    }
  }
}
