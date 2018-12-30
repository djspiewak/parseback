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
package ast

object FilterSpecs extends ParsebackSpec {

  implicit val W = Whitespace("" | """\s+""".r)

  "ast filtering" should {
    "disambiguate left-associativity" in {
      lazy val expr: Parser[Expr] = (
          expr ~ "+" ~ expr ^^ { (_, e1, _, e2) => Add(e1, e2) }
        | num               ^^ { (_, n) => IntLit(n) }
      ) filter prec(Add)

      expr must parseOk("1")(IntLit(1))
      expr must parseOk("1 + 2")(Add(IntLit(1), IntLit(2)))
      expr must parseOk("1 + 2 + 3")(Add(Add(IntLit(1), IntLit(2)), IntLit(3)))

      // TODO add a `parseLike` matcher
      /*forAll { num: Int =>
        expr(1 to ((num % 15) + 1) mkString " + ").lengthCompare(1) mustEqual 0
      }*/
    }

    "disambiguate right-associativity" in {
      lazy val expr: Parser[Expr] = (
          expr ~ "+" ~ expr ^^ { (_, e1, _, e2) => AddRight(e1, e2) }
        | num               ^^ { (_, n) => IntLit(n) }
      ) filter prec(AddRight)

      expr must parseOk("1")(IntLit(1))
      expr must parseOk("1 + 2")(AddRight(IntLit(1), IntLit(2)))
      expr must parseOk("1 + 2 + 3")(AddRight(IntLit(1), AddRight(IntLit(2), IntLit(3))))

      /*forAll { num: Int =>
        expr(1 to ((num % 15) + 1) mkString " + ").lengthCompare(1) mustEqual 0
      }*/
    }

    "disambiguate binary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ "+" ~ expr ^^ { (_, e1, _, e2) => Add(e1, e2) }
        | expr ~ "-" ~ expr ^^ { (_, e1, _, e2) => Sub(e1, e2) }
        | num               ^^ { (_, n) => IntLit(n) }
      ) filter prec(Add, Sub)

      expr must parseOk("1")(IntLit(1))
      expr must parseOk("1 + 2")(Add(IntLit(1), IntLit(2)))
      expr must parseOk("1 - 2")(Sub(IntLit(1), IntLit(2)))
      expr must parseOk("1 + 2 - 3")(Sub(Add(IntLit(1), IntLit(2)), IntLit(3)))
      expr must parseOk("1 - 2 + 3")(Sub(IntLit(1), Add(IntLit(2), IntLit(3))))
    }

    "disambiguate unary and binary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ "+" ~ expr ^^ { (_, e1, _, e2) => Add(e1, e2) }
        | "-" ~ expr        ^^ { (_, _, e) => Neg(e) }
        | num               ^^ { (_, n) => IntLit(n) }
      ) filter prec(Neg, Add)

      expr must parseOk("1")(IntLit(1))
      expr must parseOk("-1")(Neg(IntLit(1)))
      expr must parseOk("1 + 2")(Add(IntLit(1), IntLit(2)))
      expr must parseOk("-1 + 2")(Add(Neg(IntLit(1)), IntLit(2)))
      expr must parseOk("1 + -2")(Add(IntLit(1), Neg(IntLit(2))))
    }

    "disambiguate prefix unary and binary operations when binary has higher precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ "+" ~ expr ^^ { (_, e1, _, e2) => Add(e1, e2) }
        | "-" ~ expr        ^^ { (_, _, e) => Neg(e) }
        | num               ^^ { (_, n) => IntLit(n) }
      ) filter prec(Add, Neg)

      expr must parseOk("1")(IntLit(1))
      expr must parseOk("-1")(Neg(IntLit(1)))
      expr must parseOk("1 + 2")(Add(IntLit(1), IntLit(2)))
      expr must parseOk("-1 + 2")(Neg(Add(IntLit(1), IntLit(2))))
      expr must parseOk("1 + -2")(Add(IntLit(1), Neg(IntLit(2))))
    }

    "disambiguate suffix unary and binary operations when binary has higher precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ "+" ~ expr ^^ { (_, e1, _, e2) => Add(e1, e2) }
        | expr ~ "~"        ^^ { (_, e, _) => Comp(e) }
        | num               ^^ { (_, n) => IntLit(n) }
      ) filter prec(Add, Comp)

      expr must parseOk("1")(IntLit(1))
      expr must parseOk("1~")(Comp(IntLit(1)))
      expr must parseOk("1 + 2")(Add(IntLit(1), IntLit(2)))
      expr must parseOk("1 + 2~")(Comp(Add(IntLit(1), IntLit(2))))
      expr must parseOk("1~ + 2")(Add(Comp(IntLit(1)), IntLit(2)))
    }

    "ignore relative precedence of unary operations of the same fixity" in {
      lazy val expr: Parser[Expr] = (
          "-" ~ expr          ^^ { (_, _, e) => Neg(e) }
        | "~" ~ expr          ^^ { (_, _, e) => Comp2(e) }
        | "(" ~> expr <~ ")"
        | num                 ^^ { (_, n) => IntLit(n) }
      ) filter prec(Add, Neg, Comp2)

      expr must parseOk("-~1")(Neg(Comp2(IntLit(1))))
      expr must parseOk("~-1")(Comp2(Neg(IntLit(1))))
    }

    "disambiguate non-uniform fixity unary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          "-" ~ expr ^^ { (_, _, e) => Neg(e) }
        | expr ~ "~" ^^ { (_, e, _) => Comp(e) }
        | num        ^^ { (_, n) => IntLit(n) }
      ) filter prec(Comp, Neg)

      expr must parseOk("1")(IntLit(1))
      expr must parseOk("-1")(Neg(IntLit(1)))
      expr must parseOk("1~")(Comp(IntLit(1)))
      expr must parseOk("-1~")(Neg(Comp(IntLit(1))))
    }

    "disambiguate uniform associativity operations with precedence levels" in {
      lazy val expr: Parser[Expr] = (
          expr ~ "+" ~ expr ^^ { (_, e1, _, e2) => Add(e1, e2) }
        | expr ~ "-" ~ expr ^^ { (_, e1, _, e2) => Sub(e1, e2) }
        | expr ~ "*" ~ expr ^^ { (_, e1, _, e2) => Mul(e1, e2) }
        | num               ^^ { (_, n) => IntLit(n) }
      ) filter prec(Mul, (Add, Sub))

      expr must parseOk("1 + 2")(Add(IntLit(1), IntLit(2)))
      expr must parseOk("1 - 2")(Sub(IntLit(1), IntLit(2)))
      expr must parseOk("1 + 2 - 3")(Sub(Add(IntLit(1), IntLit(2)), IntLit(3)))
      expr must parseOk("1 - 2 + 3")(Add(Sub(IntLit(1), IntLit(2)), IntLit(3)))
      expr must parseOk("1 + 2 * 3")(Add(IntLit(1), Mul(IntLit(2), IntLit(3))))
    }

    "disambiguate left-associativity with explicit parenthesis" in {
      lazy val expr: Parser[Expr] = (
        expr ~ "+" ~ expr ^^ { (_, e1, _, e2) => Add(e1, e2) }
          | "(" ~> expr <~ ")"
          | num               ^^ { (_, n) => IntLit(n) }
        ) filterLeaveOne prec(Add)

      expr must parseOk("1 + (2 + 3)")(Add(IntLit(1), Add(IntLit(2), IntLit(3))))
    }

  }

  // %%

  val num = """\d+""".r map { _.toInt }

  // %%

  sealed trait Expr extends Node

  case class Add(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = 'add
  }

  case class AddRight(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = false
    val sym = 'add
  }

  case class Sub(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = 'sub
  }

  case class Mul(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = 'mul
  }

  case class Div(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = 'div
  }

  case class Neg(child: Expr) extends Expr with UnaryNode {
    val isPrefix = true
    val sym = 'neg
  }

  case class Comp2(child: Expr) extends Expr with UnaryNode {
    val isPrefix = true
    val sym = 'comp
  }

  case class Comp(child: Expr) extends Expr with UnaryNode {
    val isPrefix = false
    val sym = 'comp
  }

  case class IntLit(i: Int) extends Expr with LeafNode
}
