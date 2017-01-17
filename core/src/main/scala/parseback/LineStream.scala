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

import shims.{Applicative, Monad}

// a head-tail stream of [[Line]]s, with the tail computed in effect F[_]
sealed trait LineStream[F[+_]] extends Product with Serializable {
  import LineStream._

  /**
   * Eliminates *leading* empty lines.  Once a non-empty line is hit,
   * the remainder of the stream is ignored.  This is lazy within the
   * monad, and thus does not force IO.
   */
  final def normalize(implicit F: Monad[F]): F[LineStream[F]] = this match {
    case More(line, tail) if line.isEmpty =>
      F.flatMap(tail) { _.normalize }

    case m @ More(_, _) => F point m

    case Empty() => F point Empty()
  }
}

object LineStream {

  def apply[F[+_]: Applicative](str: String): LineStream[F] = {
    val splits = str split """\r|\r?\n"""
    val (front, last) = splits splitAt (splits.length - 1)

    apply((front map { _ + "\n" }) ++ last)
  }

  def apply[F[+_]: Applicative](lines: Seq[String]): LineStream[F] = {
    val actuals = lines.zipWithIndex map {
      case (str, lineNo) => Line(str, lineNo, 0)
    }

    actuals.foldRight(Empty(): LineStream[F]) { (line, tail) =>
      More(line, Applicative[F].point(tail))
    }
  }

  final case class More[F[+_]](line: Line, tail: F[LineStream[F]]) extends LineStream[F]
  final case class Empty[F[+_]]() extends LineStream[F]
}
