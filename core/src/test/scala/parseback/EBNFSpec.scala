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

object EBNFSpec extends ParsebackSpec {
  import ParseError._
  implicit val lexer = LexerHelper.lexer(None, Set.empty, Set.empty, Set("abc".r))

  "p?" should {
    val p = literal("abc")?

    "parse empty" in {
      p must parseOk("")(None)
    }

    "parse single" in {
      p must parseOk("abc")(Some("abc"))
    }

    "reject multi" in {
      p must failToParse("abcabcabc")(UnexpectedTrailingCharacters(Line(Token("abc", "abc", "abc"), 0, 1)))
    }
  }

  "p*" should {
    val p = literal("abc")*

    "parse empty" in {
      p must parseOk("")(Nil)
    }

    "parse one" in {
      p must parseOk("abc")("abc" :: Nil)
    }

    "parse three" in {
      p must parseOk("abcabcabc")("abc" :: "abc" :: "abc" :: Nil)
    }
  }

  "p+" should {
    val p = literal("abc")+

    "reject empty" in {
      p must failToParse("")(UnexpectedEOF(Set("abc")))
    }

    "parse one" in {
      p must parseOk("abc")("abc" :: Nil)
    }

    "parse three" in {
      p must parseOk("abcabcabc")("abc" :: "abc" :: "abc" :: Nil)
    }
  }
}
