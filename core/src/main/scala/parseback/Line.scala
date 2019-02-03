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

/**
 * @param base The full contents of the line
 * @param lineNo The line offset of this line within the larger input stream (0 indexed)
 * @param colNo The column offset into `base` (0 indexed)
 */
final case class Line(base: Array[Token], lineNo: Int = 0, colNo: Int = 0) {

  def head: Token = base(colNo)

  def project: String = base.map(_.value).mkString(" ")

  def isEmpty: Boolean = base.length == colNo

  def next: Option[Line] =
    Some(Line(base, lineNo, colNo + 1)) filter (!_.isEmpty)

  def isBefore(that: Line): Boolean =
    this.lineNo < that.lineNo || (this.lineNo == that.lineNo && this.colNo < that.colNo)

  def renderError: String =
    project + s"${0 until colNo map { _ => ' ' } mkString}^"

  // due to Array.
  override def equals(thatGeneric: scala.Any): Boolean = {
    if(!thatGeneric.isInstanceOf[Line])
      return false

    val that = thatGeneric.asInstanceOf[Line]
    val thisBase = if(this.base == null) null else this.base.deep
    val thatBase = if(that.base == null) null else that.base.deep

    (thisBase, lineNo, colNo) == ((thatBase, that.lineNo, that.colNo))
  }

  override def toString: String = s"Line(${project}, ${lineNo}, ${colNo})"
}

object Line extends ((Array[Token], Int, Int) => Line) {

  def addTo(lines: Vector[Line], line: Line): Vector[Line] = {
    if (lines.isEmpty) {
      Vector(line)
    } else {
      val last = lines.last

      if (last.lineNo < line.lineNo)
        lines :+ line
      else if (lines.length == 1)
        lines
      else
        lines.updated(lines.length - 1, line)
    }
  }
}
