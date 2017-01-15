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

  // external syntax

  type ~[+A, +B] = (A, B)
  val ~ = Tuple2

  implicit def literal(str: String)(implicit W: Whitespace): Parser[String] =
    Parser.Literal(str, 0)

  implicit def literalLazy(str: String)(implicit W: Whitespace): LazyParserSyntax[String] =
    LazyParserSyntax(literal(str))

  implicit def literalEager(str: String)(implicit W: Whitespace): EagerParser1Syntax[String] =
    EagerParser1Syntax(literal(str))

  implicit def regex(r: SRegex)(implicit W: Whitespace): Parser[String] =
    Parser.Regex(r)

  implicit def regexLazy(r: SRegex)(implicit W: Whitespace): LazyParserSyntax[String] =
    LazyParserSyntax(regex(r))

  implicit def regexEager(r: SRegex)(implicit W: Whitespace): EagerParser1Syntax[String] =
    EagerParser1Syntax(regex(r))

  implicit def unit(u: Unit): Parser[Unit] =
    Parser.Epsilon(())

  implicit def unitLazy(u: Unit): LazyParserSyntax[Unit] =
    LazyParserSyntax(unit(u))

  implicit def unitEager(u: Unit): EagerParser1Syntax[Unit] =
    EagerParser1Syntax(unit(u))

  final implicit class LazyParserSyntax[A](self: => Parser[A]) {
    def |(that: => Parser[A]): Parser[A] = Parser.Union(() => self, () => that)
  }

  // ^^ syntax

  final implicit class EagerParser1Syntax[A](val self: Parser[A]) extends AnyVal {
    def ^^[R](f: (List[Line], A) => R): Parser[R] = self mapWithLines f
  }

  final implicit class EagerParser2Syntax[A, B](val self: Parser[A ~ B]) extends AnyVal {

    def ^^[R](f: (List[Line], A, B) => R): Parser[R] = {
      self mapWithLines {
        case (lines, a ~ b) => f(lines, a, b)
      }
    }
  }

  final implicit class EagerParser3LSyntax[A, B, C](val self: Parser[A ~ B ~ C]) extends AnyVal {

    def ^^[R](f: (List[Line], A, B, C) => R): Parser[R] = {
      self mapWithLines {
        case (lines, a ~ b ~ c) => f(lines, a, b, c)
      }
    }
  }

  implicit def EagerParser3RSyntax[A, B, C](self: Parser[A ~ (B ~ C)]) =
    EagerParser3LSyntax(self map { case a ~ (b ~ c) => ((a, b), c) })

  final implicit class EagerParser4LLSyntax[A, B, C, D](val self: Parser[A ~ B ~ C ~ D]) extends AnyVal {

    def ^^[R](f: (List[Line], A, B, C, D) => R): Parser[R] = {
      self mapWithLines {
        case (lines, a ~ b ~ c ~ d) => f(lines, a, b, c, d)
      }
    }
  }

  implicit def EagerParser4RLSyntax[A, B, C, D](self: Parser[A ~ (B ~ C ~ D)]) =
    EagerParser4LLSyntax(self map { case a ~ (b ~ c ~ d) => (((a, b), c), d) })

  implicit def EagerParser4LRSyntax[A, B, C, D](self: Parser[A ~ (B ~ C) ~ D]) =
    EagerParser4LLSyntax(self map { case a ~ (b ~ c) ~ d => (((a, b), c), d) })

  implicit def EagerParser4RRSyntax[A, B, C, D](self: Parser[A ~ B ~ (C ~ D)]) =
    EagerParser4LLSyntax(self map { case a ~ b ~ (c ~ d) => (((a, b), c), d) })

  final implicit class EagerParser5LLLSyntax[A, B, C, D, E](val self: Parser[A ~ B ~ C ~ D ~ E]) extends AnyVal {

    def ^^[R](f: (List[Line], A, B, C, D, E) => R): Parser[R] = {
      self mapWithLines {
        case (lines, a ~ b ~ c ~ d ~ e) => f(lines, a, b, c, d, e)
      }
    }
  }

  implicit def EagerParser5LRLSyntax[A, B, C, D, E](self: Parser[A ~ (B ~ C ~ D) ~ E]) =
    EagerParser5LLLSyntax(self map { case a ~ (b ~ c ~ d) ~ e => ((((a, b), c), d), e) })

  implicit def EagerParser5LLRSyntax[A, B, C, D, E](self: Parser[A ~ (B ~ C) ~ D ~ E]) =
    EagerParser5LLLSyntax(self map { case a ~ (b ~ c) ~ d ~ e => ((((a, b), c), d), e) })

  implicit def EagerParser5LRRSyntax[A, B, C, D, E](self: Parser[A ~ B ~ (C ~ D) ~ E]) =
    EagerParser5LLLSyntax(self map { case a ~ b ~ (c ~ d) ~ e => ((((a, b), c), d), e) })

  implicit def EagerParser5RRLSyntax[A, B, C, D, E](self: Parser[A ~ ((B ~ C ~ D) ~ E)]) =
    EagerParser5LLLSyntax(self map { case a ~ ((b ~ c ~ d) ~ e) => ((((a, b), c), d), e) })

  implicit def EagerParser5RLRSyntax[A, B, C, D, E](self: Parser[A ~ ((B ~ C) ~ D ~ E)]) =
    EagerParser5LLLSyntax(self map { case a ~ ((b ~ c) ~ d ~ e) => ((((a, b), c), d), e) })

  implicit def EagerParser5RRRSyntax[A, B, C, D, E](self: Parser[A ~ (B ~ (C ~ D) ~ E)]) =
    EagerParser5LLLSyntax(self map { case a ~ (b ~ (c ~ d) ~ e) => ((((a, b), c), d), e) })
}
