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

sealed trait ParseError extends Product with Serializable

object ParseError {
  sealed trait WithLoc extends ParseError {
    def loc: Line
  }

  sealed trait Meta extends ParseError

  /**
   * Reduces and re-sorts `errors` according to a set of somewhat-reasonable
   * semantics.  Errors *later* in the stream are strictly preferred, as are
   * errors which carry generally more information.  Meta-errors (such as
   * unbounded recursion) are deprioritized.
   *
   * Broadly, this is just a subjective clean-up function, designed to surface
   * errors that, in practice, seem to be more helpful to users in diagnosing
   * syntactic mistakes.
   */
  def prioritize(errors: List[ParseError]): List[ParseError] = {

    /*
     * 1. UnexpectedEOF sorts first
     * 2. Location-bearing errors sort second
     * 3. *Later* locations sort before earlier ones
     *
     * Additionally, duplicate errors are removed.
     */
    val sorted = errors sortWith {
      case (UnexpectedEOF(_), _) => true
      case (_, UnexpectedEOF(_)) => false

      case (left: WithLoc, right: WithLoc) =>
        !(left.loc isBefore right.loc)

      case (left: WithLoc, _) => true
      case (_, right: WithLoc) => false

      case _ => false
    } distinct    // TODO distinct doesn't take sorting into account, and thus is slow

    sorted.headOption match {
      case Some(UnexpectedEOF(_)) =>
        val eofs = sorted takeWhile { _.isInstanceOf[UnexpectedEOF] }
        val err = eofs.reduceOption[ParseError] {
          case ((UnexpectedEOF(exp1), UnexpectedEOF(exp2))) =>
            UnexpectedEOF(exp1 ++ exp2)

          case _ => sys.error("impossible")
        }

        err.toList

      case Some(head: WithLoc) =>
        val latest = sorted takeWhile {
          case e: WithLoc => e.loc == head.loc
          case _ => false
        }

        val (left, right) = latest partition {
          case UnexpectedCharacter(_, _) => true
          case _ => false
        }

        val reducedChars = left.reduceOption[ParseError] {
          case ((UnexpectedCharacter(loc, exp1), UnexpectedCharacter(_, exp2))) =>
            UnexpectedCharacter(loc, exp1 ++ exp2)

          case _ => sys.error("impossible")
        }

        reducedChars.toList ::: right

      case Some(_: Meta) =>
        sorted takeWhile { _.isInstanceOf[Meta] }

      case None => Nil
    }
  }

  final case class UnexpectedTrailingCharacters(loc: Line) extends WithLoc
  final case class UnexpectedCharacter(loc: Line, expected: Set[String]) extends WithLoc
  final case class UnexpectedEOF(expected: Set[String]) extends ParseError
  final case class UnboundedRecursion(parser: Parser[_]) extends Meta
}
