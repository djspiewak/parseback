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

import org.specs2.mutable._

object ErrorSpec extends Specification {
  import ParseError._

  "error rendering" should {
    val line = Line(Token("Lorem", "ipsum", "dolor", "sit", "amet", "\n"), lineNo = 0, colNo = 3)

    "print trailing characters" in {
      val error = UnexpectedTrailingCharacters(line)
      error.render("ErrorSpec.scala") mustEqual "ErrorSpec.scala:1: unexpected trailing characters\nLorem ipsum dolor sit amet \n   ^"
    }

    "print unexpected characters" in {
      val error = UnexpectedCharacter(line, Set("foo", "bar"))
      error.render("ErrorSpec.scala") mustEqual "ErrorSpec.scala:1: unexpected characters; expected 'foo' or 'bar'\nLorem ipsum dolor sit amet \n   ^"
    }

    "print unexpected eof" in {
      val error = UnexpectedEOF(Set("foo", "bar"))
      error.render("ErrorSpec.scala") mustEqual "ErrorSpec.scala: unexpected end-of-file; expected 'foo' or 'bar'"
    }

    "print unbounded recursion" in {
      val error = UnboundedRecursion(null)    // TODO put a real parser here
      error.render("ErrorSpec.scala") mustEqual "ErrorSpec.scala: INTERNAL ERROR (unbounded recursion)"
    }
  }
}
