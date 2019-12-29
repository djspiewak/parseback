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

package object ast {
  type Filter[-A] = A => Boolean

  def prec(levels: PrecLevel*): Filter[Node] = new Filter[Node] {
    val normalized: Seq[Set[Manifest[_]]] = levels map { l => l.specs map { _.m } }

    val dag: Map[Manifest[_], Set[Manifest[_]]] = {
      val (back, _) = normalized.foldRight((Map[Manifest[_], Set[Manifest[_]]](), Set[Manifest[_]]())) {
        case (level, (dag, acc)) =>
          (dag ++ (level map { _ -> acc }), acc ++ level)
      }

      back
    }

    val matched: Map[Manifest[_], Set[Manifest[_]]] = {
      val pairs = normalized flatMap { level =>
        level map { m => m -> level }
      }

      Map(pairs: _*)
    }

    def apply(node: Node): Boolean = {
      import FormSpec._

      val form = node.form.linearize
      val manifest = Manifest.classType(node.getClass)

      val forbidden = dag get manifest getOrElse Set()
      val peers = matched get manifest getOrElse Set()

      (form.head, form drop 1 take (form.length - 2), form.last) match {
        case (HolePart(left), middle, HolePart(right)) => {
          val leftManifest = Manifest.classType(left.getClass)
          val rightManifest = Manifest.classType(right.getClass)

          lazy val leftBeforeRight = (node.children indexOf left) < (node.children indexOf right)

          lazy val leftAssoc =
            if (peers contains leftManifest) leftBeforeRight else true

          lazy val rightAssoc =
            if (peers contains rightManifest) !leftBeforeRight else true

          lazy val checkLeft = left.form.linearize.last match {
            case HolePart(_) => !(forbidden contains leftManifest)
            case _ => true
          }

          lazy val checkRight = right.form.linearize.head match {
            case HolePart(_) => !(forbidden contains rightManifest)
            case _ => true
          }

          leftAssoc && rightAssoc && checkLeft && checkRight
        }

        // prefix unary
        case (head, middle, HolePart(child)) if head.isSimple && (middle.lastOption map { _.isSimple } getOrElse true) => {
          val childManifest = Manifest.classType(child.getClass)

          child.form.linearize.head match {
            case HolePart(_) => !(forbidden contains childManifest)
            case _ => true
          }
        }

        // suffix unary
        case (HolePart(child), middle, last) if last.isSimple && (middle.headOption map { _.isSimple } getOrElse true) => {
          val childManifest = Manifest.classType(child.getClass)

          child.form.linearize.last match {
            case HolePart(_) => !(forbidden contains childManifest)
            case _ => true
          }
        }

        // TODO additional forms

        case _ => true
      }

      // TODO additional checks
    }
  }
}
