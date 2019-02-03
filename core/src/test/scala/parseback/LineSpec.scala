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

object LineSpec extends Specification {

  val data1 = Token("Lorem", "ipsum", "dolor", "sit", "amet", "\n")
  val data2 = Token("to", "be", "or", "not", "to", "be", ",", "that", "is", "the", "question")

  "line rendering" should {
    "basically work" in {
      val line = Line(data1, lineNo = 0, colNo = 3)

      line.renderError mustEqual "Lorem ipsum dolor sit amet \n   ^"
    }
  }

  "line accumulation" should {
    import Line.addTo

    val line = Line(data1)

    "add the first line" in {
      addTo(Vector.empty, line) mustEqual Vector(line)
    }

    "retain the first line if never advanced by line" in {
      addTo(Vector(line), line.next.get) mustEqual Vector(line)
      addTo(Vector(line), line.next.get.next.get) mustEqual Vector(line)
    }

    "add the second line" in {
      val line2 = Line(data2, lineNo = 1)

      addTo(Vector(line), line2) mustEqual Vector(line, line2)
    }

    "retain the first line and the columnar-latest subsequent lines" in {
      val line2 = Line(data2, lineNo = 1)
      val line3 = line2.next.get
      val line4 = line3.next.get

      addTo(Vector(line, line2), line3) mustEqual Vector(line, line3)
      addTo(Vector(line, line3), line4) mustEqual Vector(line, line4)
    }
  }
}
