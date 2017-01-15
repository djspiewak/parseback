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

import scala.util.matching.{Regex => SRegex}

final case class Whitespace(regex: SRegex) extends AnyVal {

  private[parseback] def stripLeading(line: Line): Option[Line] = {
    if (!regex.toString.isEmpty) {
      val whitespace = regex findPrefixOf line.project

      whitespace map { w =>
        line.copy(colNo = line.colNo + w.length)
      }
    } else {
      None
    }
  }
}

object Whitespace {
  implicit val Default = Whitespace(""r)
}
