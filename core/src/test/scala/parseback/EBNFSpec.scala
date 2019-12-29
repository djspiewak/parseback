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

class EBNFSpec extends ParsebackSpec {
  import ParseError._

  "p?" should {
    val p = literal("abc")?

    "parse empty" in {
      p must parseOk("")(None)
    }

    "parse single" in {
      p must parseOk("abc")(Some("abc"))
    }

    "reject multi" in {
      p must failToParse("abcabcabc")(UnexpectedTrailingCharacters(Line("abcabcabc", 0, 3)))
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
