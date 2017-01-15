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

import scala.util.{Either, Left, Right}
import scala.util.matching.{Regex => SRegex}

package object parseback {

  // internal convenience utilities
  private[parseback] type \/[+A, +B] = Either[A, B]

  private[parseback] val -\/ = Left
  private[parseback] val \/- = Right

  // TODO macroize
  private[parseback] def trace(str: => String): Unit = {
    // println(str)
  }

  // provides 676 possible labels; should be enough for most practical purposes
  private[parseback] lazy val PossibleLabels =
    ('A' to 'Z') flatMap { a => ('A' to 'Z') map { b => a.toString + b.toString } }

  // external syntax

  type ~[+A, +B] = (A, B)
  val ~ = Tuple2

  implicit def literal(str: String)(implicit W: Whitespace): Parser[String] =
    Parser.Literal(str, 0)

  implicit def regex(r: SRegex)(implicit W: Whitespace): Parser[String] =
    Parser.Regex(r)

  implicit def unit(u: Unit): Parser[Unit] =
    Parser.Epsilon(())

  final implicit class LazyParserSyntax[A](self: => Parser[A]) {
    def |(that: => Parser[A]): Parser[A] = Parser.Union(() => self, () => that)
  }
}
