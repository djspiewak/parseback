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

package parseback.ast

final case class PrecLevel(specs: Set[ManWrap]) extends AnyVal

case object PrecLevel extends (Set[ManWrap] => PrecLevel) {
  implicit def coerce1[A](a: A)(implicit mwA: A => ManWrap): PrecLevel =
    PrecLevel(Set[ManWrap](a))

  implicit def coerce2[A,B](pair: (A, B))(implicit mwA: A => ManWrap, mwB: B => ManWrap): PrecLevel =
    PrecLevel(Set[ManWrap](pair._1, pair._2))

  implicit def coerce3[A,B,C](pair: (A, B, C))(implicit mwA: A => ManWrap, mwB: B => ManWrap, mwC: C => ManWrap): PrecLevel =
    PrecLevel(Set[ManWrap](pair._1, pair._2, pair._3))

  implicit def coerce4[A,B,C,D](pair: (A, B, C, D))(implicit mwA: A => ManWrap, mwB: B => ManWrap, mwC: C => ManWrap, mwD: D => ManWrap): PrecLevel =
    PrecLevel(Set[ManWrap](pair._1, pair._2, pair._3, pair._4))

  implicit def coerce5[A,B,C,D,E](pair: (A, B, C, D, E))(implicit mwA: A => ManWrap, mwB: B => ManWrap, mwC: C => ManWrap, mwD: D => ManWrap, mwE: E => ManWrap): PrecLevel =
    PrecLevel(Set[ManWrap](pair._1, pair._2, pair._3, pair._4, pair._5))
}

final case class ManWrap(m: Manifest[_]) extends AnyVal

case object ManWrap extends (Manifest[_] => ManWrap) {
  implicit def coerceValue0[A <: Node](v: A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion1[A <: Node](c: _ => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion2[A <: Node](c: (_, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion3[A <: Node](c: (_, _, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion4[A <: Node](c: (_, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion5[A <: Node](c: (_, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion6[A <: Node](c: (_, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion7[A <: Node](c: (_, _, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion8[A <: Node](c: (_, _, _, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion9[A <: Node](c: (_, _, _, _, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)

  implicit def coerceCompanion10[A <: Node](c: (_, _, _, _, _, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
    ManWrap(m)
}
