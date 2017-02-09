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

package parseback.util

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._

object CatenableSpec extends Specification with ScalaCheck {
  import Catenable.{+:, Syntax}
  import Prop._

  "catenable" should {
    "retain sequence semantics" in forAll { xs: List[Int] =>
      Catenable(xs: _*).toList mustEqual xs
    }

    "implement length matching list" in forAll { xs: List[Int] =>
      Catenable(xs: _*).length mustEqual xs.length
    }

    "implement isEmpty matching list" in forAll { xs: List[Int] =>
      Catenable(xs: _*).isEmpty mustEqual xs.isEmpty
    }

    "not evaluate the right when unconsing the left" in {
      var evaluated = false

      lazy val right: Catenable[Int] = {
        evaluated = true
        Catenable.empty
      }

      (Catenable(42, 24) ++ right) must beLike {
        case i +: tail =>
          i mustEqual 42
          evaluated must beFalse

          tail must beLike {
            case i2 +: tail =>
              i2 mustEqual 24
              evaluated must beFalse
          }
      }
    }
  }
}
