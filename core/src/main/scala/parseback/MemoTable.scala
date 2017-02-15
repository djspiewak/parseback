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

import java.util.HashMap

// note that there are only two implementation here, preserving bimorphic PIC
// TODO it may be possible to retain SOME results between derivations (just not those which involve Apply)
private[parseback] sealed abstract class MemoTable {

  def derived[A](from: Parser[A], c: Char, to: Parser[A]): this.type
  def derive[A](from: Parser[A], c: Char): Option[Parser[A]]

  def finished[A](target: Parser[A], results: Results.Cacheable[A]): this.type
  def finish[A](target: Parser[A]): Option[Results.Cacheable[A]]

  def recreate(): MemoTable
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

private[parseback] final class InitialMemoTable extends MemoTable {
  import MemoTable._

  // still using the single-derivation optimization here
  private val derivations: HashMap[(MemoTable, ParserId[_]), (Char, Parser[_])] = new HashMap(16)    // TODO tune capacities
  private val finishes: HashMap[(MemoTable, ParserId[_]), Results.Cacheable[_]] = new HashMap(16)

  def derived[A](from: Parser[A], c: Char, to: Parser[A]): this.type =
    derived(this, from, c, to)

  private[parseback] def derived[A](table: MemoTable, from: Parser[A], c: Char, to: Parser[A]): this.type = {
    derivations.put((table, new ParserId(from)), (c, to))

    this
  }

  def derive[A](from: Parser[A], c: Char): Option[Parser[A]] =
    derive(this, from, c)

  private[parseback] def derive[A](table: MemoTable, from: Parser[A], c: Char): Option[Parser[A]] = {
    val back = derivations.get((table, new ParserId(from)))

    if (back != null && back._1 == c)
      Some(back._2.asInstanceOf[Parser[A]])
    else
      None
  }

  def finished[A](target: Parser[A], results: Results.Cacheable[A]): this.type =
    finished(this, target, results)

  private[parseback] def finished[A](table: MemoTable, target: Parser[A], results: Results.Cacheable[A]): this.type = {
    finishes.put((table, new ParserId(target)), results)

    this
  }

  def finish[A](target: Parser[A]): Option[Results.Cacheable[A]] =
    finish(this, target)

  private[parseback] def finish[A](table: MemoTable, target: Parser[A]): Option[Results.Cacheable[A]] =
    Option(finishes.get((table, new ParserId(target))).asInstanceOf[Results.Cacheable[A]])

  def recreate(): MemoTable = new FieldMemoTable(this)
}

private[parseback] final class FieldMemoTable(delegate: InitialMemoTable) extends MemoTable {

  def derived[A](from: Parser[A], c: Char, to: Parser[A]): this.type = {
    if (from.isRoot) {
      delegate.derived(this, from, c, to)
    } else {
      from.derivedTable = this
      from.derivedC = c
      from.derivedR = to
    }

    this
  }

  def derive[A](from: Parser[A], c: Char): Option[Parser[A]] = {
    if (from.isRoot) {
      delegate.derive(this, from, c)
    } else {
      if ((from.derivedTable eq this) && from.derivedC == c)
        Option(from.derivedR)
      else
        None
    }
  }

  def finished[A](target: Parser[A], results: Results.Cacheable[A]): this.type = {
    if (target.isRoot) {
      delegate.finished(this, target, results)
    } else {
      target.finishedTable = this
      target.finished = results
    }

    this
  }

  def finish[A](target: Parser[A]): Option[Results.Cacheable[A]] = {
    if (target.isRoot) {
      delegate.finish(this, target)
    } else {
      if (target.finishedTable eq this)
        Option(target.finished)
      else
        None
    }
  }

  def recreate(): MemoTable = new FieldMemoTable(delegate)
}
