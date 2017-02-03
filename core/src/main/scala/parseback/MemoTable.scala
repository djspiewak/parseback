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

import scala.collection.mutable

// TODO this could be a TON more optimized with some specialized data structures
// TODO it may be possible to retain SOME results between derivations (just not those which involve Apply)
private[parseback] final class MemoTable {
  import MemoTable._

  // still using the single-derivation optimization here
  private val derivations: mutable.Map[ParserId[_], (Char, Parser[_])] = mutable.Map()
  private val finishes: mutable.Map[ParserId[_], Results.Cacheable[_]] = mutable.Map()

  def derived[A](from: Parser[A], c: Char, to: Parser[A]): this.type = {
    derivations(new ParserId(from)) = (c, to)

    this
  }

  def derive[A](from: Parser[A], c: Char): Option[Parser[A]] = {
    val id = new ParserId(from)

    derivations get id filter { _._1 == c } map { _._2.asInstanceOf[Parser[A]] }
  }

  def finished[A](target: Parser[A], results: Results.Cacheable[A]): this.type = {
    finishes(new ParserId(target)) = results

    this
  }

  def finish[A](target: Parser[A]): Option[Results.Cacheable[A]] = {
    val id = new ParserId(target)

    finishes get id map { _.asInstanceOf[Results.Cacheable[A]] }
  }
}

private[parseback] object MemoTable {

  final class ParserId[A](val self: Parser[A]) {

    override def equals(that: Any) = that match {
      case that: ParserId[_] => this.self eq that.self
      case _ => false
    }

    override def hashCode = System.identityHashCode(self)
  }
}
