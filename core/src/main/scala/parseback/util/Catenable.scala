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

import scala.annotation.tailrec
import scala.collection.mutable

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
      val rights = new mutable.ListBuffer[() => Catenable[A]]

      @tailrec
      def loop(self: Catenable[A]): Option[A] = self match {
        case self @ Append(_, _) =>
          rights += self._right
          loop(self.left)

        case Single(value) => Some(value)
        case Empty => None
      }

      loop(self.left) map { a =>
        val tail = rights.foldRight(self._right) { (l, r) => () => Append(l, r) }

        (a, tail())
      }

    case Single(value) => Some((value, Empty))
    case Empty => None
  }

  final def toList: List[A] =
    uncons map { case (hd, tail) => hd :: tail.toList } getOrElse Nil
}

object Catenable {

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
