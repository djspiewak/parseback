/*
 * Copyright 2019 Daniel Spiewak
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

class CompoundParserSpec extends ParsebackSpec {
  import ParseError._

  "compound non-terminal parsers" should {
    "parse an unambiguous right-recursive grammar" in {
      lazy val p: Parser[String] = (
          "a" ~ p ^^ { (_, a, p) => a + p }
        | "a"
      )

      p must not(throwA[Throwable])

      p must recognize("a")
      p must recognize("aa")
      p must recognize((0 to 256).foldLeft("") { (s, _) => s + "a" })
      p must recognize("aaaaa")
    }

    "parse an LL(1) right-recursive grammar" in {
      lazy val p: Parser[String] = (
          "a" ~ p ^^ { (_, a, p) => a + p }
        | "b"
      )

      p must not(throwA[Throwable])

      p must recognize("b")
      p must recognize("ab")
      p must recognize((0 to 256).foldLeft("") { (s, _) => s + "a" } + "b")
      p must recognize("aaaab")
    }

    "parse an unambiguous left-recursive grammar (recursive-major)" in {
      lazy val p: Parser[String] = (
          p ~ "a" ^^ { (_, p, a) => p + a }
        | "a"
      )

      p must not(throwA[Throwable])

      p must recognize("a")
      p must recognize("aa")
      p must recognize((0 to 256).foldLeft("") { (s, _) => s + "a" })
      p must recognize("aaaaa")
    }

    "parse an unambiguous left-recursive grammar (recursive-minor)" in {
      lazy val p: Parser[String] = (
          "a"
        | p ~ "a" ^^ { (_, p, a) => p + a }
      )

      p must not(throwA[Throwable])

      p must recognize("a")
      p must recognize("aa")
      p must recognize((0 to 256).foldLeft("") { (s, _) => s + "a" })
      p must recognize("aaaaa")

      p must failToParse("")(UnexpectedEOF(Set("a")))
    }

    "parse an unambiguous hidden left-recursive grammar" in {
      lazy val p: Parser[Any] = (
          (() | "a") ~ p ~ "b"
        | "b"
      )

      p must recognize("aabbb")
      p must recognize("abbb")
      p must recognize("bbbbbb")
    }

    "not bug out randomly" in {
      lazy val c: Parser[Any] = (
          "b"
        | (() | "a") ~ c ~ "c"
      )

      val s = c ~ "a"

      s must recognize("bca")
    }

    "parse Gamma_1" in {
      lazy val c: Parser[Any] = (
          "b"
        | (() | "a") ~ c ~ "b"
        | "b" ~ "b"
      )

      val s = (
          c ~ "a"
        | "d"
      )

      s must not(throwA[Throwable])

      s must recognize("abba")
      s must recognize("bbbbbbba")
      s must recognize("d")
      s must recognize("aaaaaaaabbbbbbbbba")

      val prefix = (0 to 20).foldLeft("") { (str, _) => str + "a" }
      val suffix = (0 to 50).foldLeft("") { (str, _) => str + "b" }
      s must recognize(prefix + suffix + "a")

      s must recognize("bba")
    }

    "parse Gamma_2" in {
      lazy val s: Parser[String] = (
          "b"
        | s ~ s     ^^ { (_, s1, s2) => s1 + s2 }
        | s ~ s ~ s ^^ { (_, s1, s2, s3) => s1 + s2 + s3 }
      )

      s must not(throwA[Throwable])

      s must recognize("b")
      s must recognize("bb")

      s must recognize((0 to 50).foldLeft("") { (str, _) => str + "b" })

      s must recognize("bbbbb")
    }

    "parse Gamma_2*" in {
      lazy val s: Parser[String] = (
          "b"
        | s ~ s ~ (s | () ^^^ "") ^^ { (_, s1, s2, s3) => s1 + s2 + s3 }
      )

      s must not(throwA[Throwable])

      s must recognize("b")
      s must recognize("bb")
      s must recognize((0 to 50).foldLeft("") { (str, _) => str + "b" })
      s must recognize("bbbbb")
    }

    "parse an unambiguous arithmetic grammar" in {
      implicit val W = Whitespace({
        implicitly[Whitespace] mustEqual Whitespace.Default

        lazy val p: Parser[Any] = () | p ~ """\s+""".r

        p
      })

      lazy val expr: Parser[Int] = (
          expr ~ "+" ~ term     ^^ { (_, e1, _, e2) => e1 + e2 }
        | expr ~ "-" ~ term     ^^ { (_, e1, _, e2) => e1 - e2 }
        | term
      )

      lazy val term: Parser[Int] = (
          term ~ "*" ~ factor   ^^ { (_, e1, _, e2) => e1 * e2 }
        | term ~ "/" ~ factor   ^^ { (_, e1, _, e2) => e1 / e2 }
        | factor
      )

      lazy val factor: Parser[Int] = (
          "(" ~> expr <~ ")"
        | "-" ~> factor         ^^ { (_, i) => -i }
        | """\d+""".r           ^^ { (_, str) => str.toInt }
      )

      val input = """
        | 1 + 6 / 3 * 2
        | - -2 + 3 /
        | (1 + 2)
        | """.stripMargin


      expr must parseOk(input)(8)
    }

    "avoid greedy matching on a local ambiguity" in {
      sealed trait Expr

      case class Binding(formals: Vector[String]) extends Expr
      case class TicVar(id: String) extends Expr
      case class Dispatch(actuals: Vector[Expr]) extends Expr

      implicit val W = Whitespace(() | """\s+""".r)

      lazy val expr: Parser[Expr] = (
          "(" ~ formals ~ ")" ~ ":=" ^^ { (_, _, fs, _, _) => Binding(fs) }
        | "(" ~ expr ~ ")" ^^ { (_, _, as, _) => Dispatch(Vector(as)) }
        | ticId ^^ { (_, id) => TicVar(id) }
      )

      lazy val formals: Parser[Vector[String]] = (
          formals ~ "," ~ ticId ^^ { (_, fs, _, f) => fs :+ f }
        | ticId                 ^^ { (_, id) => Vector(id) }
      )

      lazy val ticId = "a"

      expr must recognize("(a) :=")
    }

    "handle nested left-recursion" in {
      implicit val W = Whitespace(() | """\s+""".r)

      lazy val exp: Parser[Any] = (
          n
        | "(" ~ commaExps ~ ")"
        | exp ~ exp
      ) ^^^ null

      lazy val commaExps: Parser[Any] = (
          exp
        | commaExps ~ "," ~ exp
      )

      lazy val n = """\d+"""r

      exp must recognize("(0,0) 2")
    }

    // commented out until negation classes are implemented
    /*"negate using a terminal parser" in {
      val p1 = "test" \ "test"

      p1("test") must beLike {
        case Failure(SyntaxError, LineStream(tail @ _*)) #:: SNil =>
          tail.mkString mustEqual "test"
      }

      val p2 = "test" \ "ing"

      p2("test") must beLike {
        case Success("test", LineStream()) #:: SNil => ok
      }
    }

    "negate within a sequence" in {
      import RegexParsers._

      // Formerly:
      // val p = ("a|b".r \ "a") ~ "c" ^^ (_ + _)
      //
      // The explicit call to RegexParsers.funSyntax2 is necessary in 2.10 to
      // deal with the consequences of implicit ambiguity: we are in a class
      // which inherits from Parsers (and so inherits an implicit funSyntax2)
      // but within a nested scope, importing RegexParsers._ brings in another
      // implicit funSyntax2, and that's the one we need.
      //
      // The change in 2.10, which is at least ostensibly a bug fix, is to
      // consistently apply the rule that if a method cannot be called explicitly,
      // then it cannot be called implicitly. And indeed in both 2.9 and 2.10,
      // an attempt to call funSyntax2 without qualification is an error:
      //
      // [error] CompoundSpecs.scala:359: reference to funSyntax2 is ambiguous;
      // [error] it is both defined in trait Parsers and imported subsequently by
      // [error] import RegexParsers._
      // [error]       val p = /*RegexParsers.*/funSyntax2(("a|b".r \ "a") ~ "c") ^^ (_ + _)
      // [error]                                ^
      //
      // Unfortunately rather than reporting the ambiguity, it reports its
      // confusion at trying to type "_ + _" when it wasn't able to work out
      // what method is being called with that as an argument. So it would
      // articulate the issue like this:
      //
      // [error] CompoundSpecs.scala:359: missing parameter type for expanded function ((x$53, x$54) => x$53.$plus(x$54))
      // [error]       val p = ("a|b".r \ "a") ~ "c" ^^ (_ + _)
      // [error]                                         ^
      //
      // The fully qualified, works in 2.9 and 2.10 formulation:
      val p = RegexParsers.funSyntax2(("a|b".r \ "a") ~ "c") ^^ (_ + _)

      p("bc") must beLike {
        case Success("bc", LineStream()) #:: SNil => ok
      }

      p("ac") must beLike {
        case Failure(SyntaxError, _) #:: _ => ok
      }
    }*/

    "correctly globally disambiguate a local sequence ambiguity" in {
      lazy val expr: Parser[Any] = (
          id ~ ":=" ~ expr ~ expr
        | num
      )

      lazy val id = "a" | "b" | "c"
      lazy val num = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"

      expr must recognizeUnambiguously("a:=1c:=23")
    }

    "successfully process a peculiar mutual left-recursion" in {
      lazy val expr: Parser[Any] = (
          prefix ~ "b"
        | "a"
        | expr ~ "+"
      )

      lazy val prefix: Parser[Any] = (
          prefix ~ ","
        | expr ~ ","
      )

      expr must recognize("a,,b+")
    }

    "parse a reduced expression form without missing the primary terms" in {
      lazy val expr: Parser[Int] = (
          expr <~ "+" ^^ { (_, e) => e + 1 }
        | term
      )

      lazy val term: Parser[Int] = (
          term <~ "*" ^^ { (_, e) => e * 3 }
        | "1" ^^^ 1
      )

      expr must parseOk("1+")(2)
    }

    "globally disambiguate a local sequence ambiguity" in {
      implicit val W = Whitespace(() | """\s+""".r)

      lazy val expr: Parser[Any] = (
          id ~ ":=" ~ expr ~ expr
        | num
        | id
      )

      lazy val id = """[a-zA-Z]""".r
      lazy val num = """\d+""".r

      expr must recognize("a := 1 c := 2 3", ambiguous = false)
    }
  }

  // commented out until we implement EBNF operators
  /*"repeated non-terminal parsers" should {
    "repeat 0..n times" in {
      val p = literal("123")*

      p("123") must beLike {
        case Success(List("123"), LineStream()) #:: SNil => ok
      }

      p("123123123123123123123123123123123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), LineStream()) #:: SNil => ok
      }

      p(LineStream()) must beLike {
        case Success(Nil, LineStream()) #:: SNil => ok
      }
    }
    "repeat 0..n times with separator" in {
      val p = literal("123") * ","

      p("123") must beLike {
        case Success(List("123"), LineStream()) #:: SNil => ok
      }

      p("123,123,123,123,123,123,123,123,123,123,123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), LineStream()) #:: SNil => ok
      }

      p(LineStream()) must beLike {
        case Success(Nil, LineStream()) #:: SNil => ok
      }
    }

    "repeat 1..n times" in {
      val p = literal("123")+

      p("123") must beLike {
        case Success(List("123"), LineStream()) #:: SNil => ok
      }

      p("123123123123123123123123123123123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), LineStream()) #:: SNil => ok
      }
    }

    "repeat 1..n times with separator" in {
      val p = literal("123") + ","

      p("123") must beLike {
        case Success(List("123"), LineStream()) #:: SNil => ok
      }

      p("123,123,123,123,123,123,123,123,123,123,123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), LineStream()) #:: SNil => ok
      }
    }

    "repeat 0..1 times" in {
      val p = literal("123")?

      p("123") must beLike {
        case Success(Some("123"), LineStream()) #:: SNil => ok
      }

      p(LineStream()) must beLike {
        case Success(None, LineStream()) #:: SNil => ok
      }
    }

    "repeat 1..n times with optional parser as first component" in {
      val p = ("-".? ~> "1")+

      p("11") must beLike {
        case Success(List("1", "1"), LineStream()) #:: SNil => ok
      }
      p("-11") must beLike {
        case Success(List("1", "1"), LineStream()) #:: SNil => ok
      }
      p("1-1") must beLike {
        case Success(List("1", "1"), LineStream()) #:: SNil => ok
      }
      p("-1-1") must beLike {
        case Success(List("1", "1"), LineStream()) #:: SNil => ok
      }
      p("1-1-1") must beLike {
        case Success(List("1", "1", "1"), LineStream()) #:: SNil => ok
      }
    }
  }*/
}
