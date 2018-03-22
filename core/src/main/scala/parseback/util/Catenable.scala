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

import cats.{Applicative, Eval, Monad, Monoid, StackSafeMonad, Traverse}

import scala.annotation.tailrec

sealed trait Catenable[+A] {
  import Catenable._

  final def +:[B >: A](b: B): Catenable[B] =
    Append(() => Single(b), () => this)

  /**
   * Ammortizes O(1) since we reconstruct the tails in such a way
   * that subsequent unconses take literally constant time.  We
   * only perform a logarithmic number of reconstructions when
   * unconsing the whole catenable.  Also note that thunks are only
   * forced as required to view structure (i.e. left-biasing).
   */
  final def uncons: Option[(A, Catenable[A])] = this match {
    case self @ Append(_, _) =>
      self.left match {

        // fast-path for when we're already left-biased
        // this short-circuit isn't necessary (it will
        // happen in the other case as well), it's just
        // a little faster since it doesn't allocate a
        // new ListBuffer
        case Single(head) =>
          Some((head, self.right))

        case _ =>
          type Thunk = () => Catenable[A]

          @tailrec
          def backtrack(rights: List[Thunk]): Option[(List[Thunk], A)] = {
            rights match {
              case self :: tail =>
                self() match {
                  case self @ Append(_, _) => deconstruct(self, tail)
                  case Single(value) => Some((tail, value))
                  case Empty => backtrack(tail)
                }

              case Nil => None
            }
          }

          @tailrec
          def deconstruct(self: Catenable[A], rights: List[Thunk]): Option[(List[Thunk], A)] = {
            self match {
              case self @ Append(_, _) =>
                deconstruct(self.left, { () => self.right } :: rights)

              case Single(value) => Some((rights, value))
              case Empty => backtrack(rights)
            }
          }

          deconstruct(self.left, List(() => self.right)) map {
            case (rights, a) =>
              val tailOption = rights reduceRightOption { (l, r) =>
                () => Append(l, r)
              }

              val tail = tailOption getOrElse { () => Empty }

              (a, tail())
          }
      }

    case Single(value) => Some((value, Empty))
    case Empty => None
  }

  final def filter(p: A => Boolean): Catenable[A] = this match {
    case self @ Append(_, _) =>
      Append(() => self.left filter p, () => self.right filter p)

    case Single(value) if p(value) => this
    case Single(_) => Empty
    case Empty => Empty
  }

  final def collect[B](p: PartialFunction[A, B]): Catenable[B] = this match {
    case self @ Append(_, _) =>
      Append(() => self.left collect p, () => self.right collect p)

    case Single(value) if p isDefinedAt value => Single(p(value))
    case Single(_) => Empty
    case Empty => Empty
  }

  final def map[B](f: A => B): Catenable[B] = this match {
    case self @ Append(_, _) =>
      Append(() => self.left map f, () => self.right map f)

    case Single(value) => Single(f(value))
    case Empty => Empty
  }

  final def flatMap[B](f: A => Catenable[B]): Catenable[B] = this match {
    case self @ Append(_, _) => Append(() =>
      self.left flatMap f, () => self.right flatMap f)

    case Single(value) => f(value)
    case Empty => Empty
  }

  /**
   * Forces the structure in G[_].
   */
  final def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[Catenable[B]] = this match {
    case self @ Append(_, _) =>
      val left2 = self.left traverse f
      val right2 = self.right traverse f

      val rightF = G.map(right2) { c1 => c2: Catenable[B] =>
        c1 ++ c2
      }

      G.ap(rightF)(left2)

    case Single(value) => G.map(f(value)) { Single(_) }
    case Empty => G point Empty
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    uncons match {
      case Some((a, tail)) =>
        tail.foldLeft(f(z, a))(f)

      case None =>
        z
    }
  }

  def foldRight[B](lz: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    Eval defer {
      uncons match {
        case Some((a, tail)) =>
          f(a, tail.foldRight(lz)(f))

        case None =>
          lz
      }
    }
  }

  final def isEmpty: Boolean = this match {
    case self @ Append(_, _) => self.left.isEmpty && self.right.isEmpty
    case Single(_) => false
    case Empty => true
  }

  final def length: Int = this match {
    case self @ Append(_, _) => self.left.length + self.right.length
    case Single(_) => 1
    case Empty => 0
  }

  final def toList: List[A] =
    uncons map { case (hd, tail) => hd :: tail.toList } getOrElse Nil
}

object Catenable {

  implicit val monad: Monad[Catenable] with Traverse[Catenable] = new Monad[Catenable] with Traverse[Catenable] with StackSafeMonad[Catenable] {

    def pure[A](a: A) = Single(a)

    def flatMap[A, B](c: Catenable[A])(f: A => Catenable[B]) =
      c flatMap f

    override def map[A, B](c: Catenable[A])(f: A => B) =
      c map f

    def traverse[G[_]: Applicative, A, B](c: Catenable[A])(f: A => G[B]): G[Catenable[B]] =
      c traverse f

    def foldLeft[A, B](fa: Catenable[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    def foldRight[A, B](fa: Catenable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.foldRight(lb)(f)
  }

  implicit def monoid[A]: Monoid[Catenable[A]] = new Monoid[Catenable[A]] {

    def empty = Empty

    def combine(left: Catenable[A], right: Catenable[A]) =
      left ++ right
  }

  def empty[A]: Catenable[A] = Empty

  def apply[A](xs: A*): Catenable[A] = {
    if (xs.isEmpty)
      Empty
    else if (xs.lengthCompare(1) == 0)
      Single(xs.head)
    else
      xs.foldRight(empty[A]) { _ +: _ }
  }

  object +: {
    def unapply[A](self: Catenable[A]): Option[(A, Catenable[A])] =
      self.uncons
  }

  implicit class Syntax[A](left: => Catenable[A]) {

    def ++[B >: A](right: => Catenable[B]): Catenable[B] =
      Append(() => left, () => right)
  }

  case object Empty extends Catenable[Nothing]
  final case class Single[+A](value: A) extends Catenable[A]

  final case class Append[+A](_left: () => Catenable[A], _right: () => Catenable[A]) extends Catenable[A] {
    lazy val left = _left()
    lazy val right = _right()
  }
}
