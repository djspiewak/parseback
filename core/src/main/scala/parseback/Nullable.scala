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

import Nullable.{ False, Maybe, True }

// a Kleene algebra
private[parseback] final class Nullable(val value: Byte) extends AnyVal {
  def ||(that: Nullable): Nullable = (this: @unchecked) match {
    case True  => True
    case Maybe => if (that == True) True else Maybe
    case False => that
  }

  def &&(that: Nullable): Nullable = (this: @unchecked) match {
    case True  => that
    case Maybe => if (that == False) False else Maybe
    case False => False
  }

  def toBoolean: Boolean = (this: @unchecked) match {
    case True  => true
    case Maybe => sys.error("not intended to be called")
    case False => false
  }

  override def toString = (this: @unchecked) match {
    case True  => "True"
    case Maybe => "Maybe"
    case False => "False"
  }
}

private[parseback] object Nullable {
  val True  = new Nullable(1)
  val Maybe = new Nullable(0)
  val False = new Nullable(-1)
}
