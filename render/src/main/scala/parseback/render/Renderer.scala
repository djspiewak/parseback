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
package render

import cats.instances.list._
import cats.instances.option._
import cats.data.State
import cats.syntax.all._

import util.EitherSyntax._

object Renderer {
  import Parser._

  type RenderState = (Map[String, (Parser[_], List[Parser[_]])], Set[String])

  // provides 676 possible labels; should be enough for most practical purposes
  private[parseback] lazy val PossibleLabels =
    ('A' to 'Z') flatMap { a => ('A' to 'Z') map { b => a.toString + b.toString } }

  // graph rendering is complicated... :-/
  final def renderCompact(self: Parser[_]): String = {
    def renderNonterminal(label: String, target: List[RenderResult.TokenSequence]): State[RenderState, String] = {
      require(!target.isEmpty)

      val init = label :: "::=" :: Nil

      def handleSequence(seq: RenderResult.TokenSequence): State[RenderState, List[String]] =
        for {
          st <- State.get[RenderState]
          (map, _) = st

          inverted = Map(map.toList map {
            case (label, (target, _)) => target -> label
          }: _*)

          rendered <- seq traverse {
            case -\/(p) =>
              if (inverted contains p) {
                State.pure[RenderState, List[String]](inverted(p) :: Nil)
              } else {
                render(p) flatMap handleSequence
              }

            case \/-(str) =>
              State.pure[RenderState, List[String]](str :: Nil)
          }
        } yield rendered.flatten

      for {
        renderedBranches <- target traverse handleSequence
        rendered = renderedBranches reduce { _ ::: "|" :: _ }
      } yield (init ::: rendered) mkString " "
    }

    val renderAll = for {
      start <- render(self)

      st <- State.get[RenderState]
      (nts, labels) = st

      // if we ARE a non-terminal, relabel as start
      relabeled <- nts find { case (_, (p, _)) => p eq this } traverse {
        case (label, (target, branches)) =>
          val nts2 = nts - label + ("𝑆" -> ((target, branches)))
          val labels2 = labels - label + "𝑆"

          State.set((nts2, labels2))
      }

      startRender <- if (relabeled.isDefined)
        State.pure[RenderState, Option[String]](None)
      else
        renderNonterminal("𝑆", start :: Nil) map { Some(_) }

      // shadow the earlier state
      st <- State.get[RenderState]
      (nts, _) = st

      allRendered <- nts.toList traverse {
        case (label, (_, branches)) =>
          for {
            tokenSequences <- branches traverse render
            rendered <- renderNonterminal(label, tokenSequences)
          } yield rendered
      }
    } yield (startRender.toList ::: allRendered) mkString " ; "

    renderAll runA ((Map(), Set())) value
  }

  def render(self: Parser[_]): State[RenderState, RenderResult.TokenSequence] = self match {
    case Sequence(left, _, right) =>
      State pure (-\/(left) :: -\/(right) :: Nil)

    case Union(_, _) =>
      for {
        st <- State.get[RenderState]
        (nts, labels) = st

        back <- if (nts.values exists { case (p, _) => p eq self }) {
          State.pure[RenderState, RenderResult.TokenSequence](-\/(self) :: Nil)
        } else {
          val (labels2, label) = assignLabel(labels)

          val branches = gatherBranches(self, None)

          for {
            _ <- State.set((nts + (label -> ((self, branches))), labels2))
          } yield -\/(self) :: Nil
        }
      } yield back

    case Apply(target, _, _) =>
      State pure (-\/(target) :: \/-("↪") :: \/-("λ") :: Nil)

    case Literal(literal, offset) =>
      State pure ((s"'${literal substring offset}'" :: Nil) map { \/-(_) })

    case Regex(r) =>
      State pure ((s"/${r.pattern}/" :: Nil) map { \/-(_) })

    case Epsilon(value) =>
      State pure ((s"ε=${value.toString}" :: Nil) map { \/-(_) })

    case Failure(errors) =>
      State pure (("!!" :: Nil) map { \/-(_) })
  }

  private def gatherBranches(self: Parser[_], root: Option[Parser[_]]): List[Parser[_]] = self match {
    case self @ Union(_, _) =>
      root match {
        case Some(p) if p eq self => self :: Nil
        case Some(_) =>
          gatherBranches(self.left, root) ::: gatherBranches(self.right, root)

        case None =>
          gatherBranches(self.left, Some(self)) ::: gatherBranches(self.right, Some(self))
      }

    case _ =>
      self :: Nil
  }

  private final def assignLabel(labels: Set[String]): (Set[String], String) = {
    val candidate = PossibleLabels(scala.util.Random.nextInt(PossibleLabels.length))

    if (labels contains candidate)
      assignLabel(labels)
    else
      (labels + candidate, candidate)
  }
}
