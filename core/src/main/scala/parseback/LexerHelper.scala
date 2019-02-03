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

import scala.util.matching.Regex

object LexerHelper {

  def lexer(whitespace: Option[Regex], keywords: Set[String], symbols: Set[String], others: Set[Regex]): String => Array[Token] = { str: String =>
    type R = (Option[Token], Int)

    def lexingW(str: String, index: Int): Option[R] = {
      whitespace.flatMap(_.findPrefixOf(str) match {
        case Some(space) =>
          Option((None, index + space.length))
        case None =>
          Option.empty
      })
    }

    def lexingR(rs: Set[Regex], str: String, index: Int): Option[R] = {
      val k = rs.flatMap(r => r.findPrefixOf(str))
      if(k.nonEmpty) {
        val m = k.maxBy(k => trim(k).length)
        Option((Some(Token(trim(m))), index + m.length))
      } else {
        Option.empty
      }
    }

    def lexingS(rs: Set[String], str: String, index: Int): Option[R] = {
      rs.filter(s => str.startsWith(s)) match {
        case s if s.nonEmpty =>
          val m = s.maxBy(_.length)
          Option((Some(Token(m)), index + m.length))
        case _ =>
          Option.empty
      }
    }

    def trim(s:String): String = {
      whitespace.map(_.replaceAllIn(s, "")).getOrElse(s)
    }

    def loop(index: Int): (Option[Token], Int) = {
      val sub = str.substring(index)
      lexingW(sub, index) getOrElse {
        lexingR(keywords.map(r => s"${r}${whitespace.map(_.pattern).getOrElse("")}".r), sub, index) getOrElse {
          lexingS(symbols, sub, index) getOrElse {
            lexingR(others, sub, index) getOrElse {
              throw new Exception(s"lexing fail at $sub")
            }
          }
        }
      }
    }
    def unfold[A, B](until: A => Boolean, h: A => (Option[B], A), a: A): List[B] =
      if (until(a))
        Nil
      else {
        h(a) match {
          case (Some(v), n) =>
            v :: unfold(until, h, n)
          case (None, n) =>
            unfold(until, h, n)
        }
      }

    if(str.isEmpty) {
      Array.empty
    } else {
      unfold[Int, Token](index => str.length <= index, loop, 0).toArray
    }
  }

}
