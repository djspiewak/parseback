/*
 * Copyright 2020 Daniel Spiewak
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

import cats.Eval

import org.specs2.matcher.Matcher
import org.specs2.mutable._
import org.specs2.specification.SpecificationFeatures

import scala.util.{Left, Right}

trait ParsebackSpec extends Spec with SpecificationFeatures {

  final def recognize[A](input: String, ambiguous: Boolean = true)(implicit W: Whitespace): Matcher[Parser[A]] = { p: Parser[A] =>
    val maybeResults = p(LineStream[Eval](input)).value

    maybeResults match {
      case Right(results) =>
        (ambiguous || results.toList.length == 1,    // TODO implement lengthCompare
          s"recognized '$input'",
          s"recognized '$input', but with multiple results: $results")

      case Left(err) =>
        (false, "", s"failed to recognize '$input' with error $err")
    }
  }

  final def recognizeUnambiguously[A](input: String) = recognize[A](input, false)

  final def parseOk[A](input: String)(results: A*)(implicit W: Whitespace): Matcher[Parser[A]] = { p: Parser[A] =>
    val maybeResults = p(LineStream[Eval](input)).value

    maybeResults match {
      case Right(actual) =>
        val actualList = actual.toList
        val permuted = actualList flatMap { a => results map { b => (a, b) } }

        val compared = permuted filter {
          case (a, b) => a == b
        }

        (results.length == actualList.length && compared.length == results.length,
          s"accepted '$input' with results $actual",
          s"accepted '$input' but produced results $actualList, expected $results")

      case Left(err) =>
        (false, "", s"failed to parse '$input' with error $err")
    }
  }

  final def failToParse(input: String)(errors: ParseError*)(implicit W: Whitespace): Matcher[Parser[_]] = { p: Parser[_] =>
    val maybeResults = p(LineStream[Eval](input)).value

    maybeResults match {
      case Right(results) =>
        (false, "", s"failed to reject '$input' with results $results")

      case Left(actual) =>
        val permuted = actual flatMap { a => errors map { b => (a, b) } }

        val compared = permuted filter {
          case (a, b) => a == b
        }

        (errors.length == actual.length && compared.length == errors.length,
          s"rejected '$input' with errors $actual",
          s"rejected '$input' with errors $actual, expected $errors")
    }
  }
}
