/*
 * fol2asp
 *
 * Copyright 2016-2019 Matthias Nickles
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package conversion

abstract class Sentence {

  def traverseFilter(b: (Sentence /*element in AST to check top-down */ , Vector[Sentence] /*path. Last element is parent of element to check*/ ,
    Int /*number of implication antecedents or negations the checked subformula occurs in */ ) => Boolean,
    path: Vector[Sentence] /*path to "this" (excluding "this"). Empty if "this" is root of AST (e.g., top-level rule)*/ ,
    noInAntecedents: Int, findMaxOne: Boolean): (Vector[(Sentence /*found subformula*/ , Vector[Sentence] /*<- path to found subformula*/ ,
    Int /*<- number of implication antecedents or negations _1 occurs in (in order to determine its polarity)*/ )] ) = {

    val tb = if (b(this, path, noInAntecedents)) Vector[(Sentence, Vector[Sentence], Int)]((this, path, noInAntecedents)) else Vector[(Sentence, Vector[Sentence], Int)]()

    val newPath = path.:+(this)

    val cltt: Vector[(Sentence, Vector[Sentence], Int)] = if (findMaxOne && !tb.isEmpty) Vector() else this match {

      // TODO: this match emulates subtype polymorphism (which would be more appropriate)

      case s: AtomicSentence => Vector[(Sentence, Vector[Sentence] /*path to ._1*/ , Int)]()

      case ConvConditional(conc, cond) => {

        val t1 = conc.traverseFilter(b, newPath, noInAntecedents, findMaxOne)

        val t2 = cond.traverseFilter(b, newPath, noInAntecedents + 1, findMaxOne)

        val cl: Vector[(Sentence, Vector[Sentence], Int)] = t1 ++ t2

        cl

      } // conc <- cond

      case Negation(s) => {  // default negation, not strong negation

        val t = s.traverseFilter(b, newPath, noInAntecedents + 1, findMaxOne)

        val cl: Vector[(Sentence, Vector[Sentence], Int)] = t

        cl

      }

      case Conditional(s1, s2) => {

        val t1 = s1.traverseFilter(b, newPath, noInAntecedents + 1, findMaxOne)

        val t2 = s2.traverseFilter(b, newPath, noInAntecedents, findMaxOne)

        val cl: Vector[(Sentence, Vector[Sentence], Int)] = t2 /*._1*/ ++ t1

        cl

      }

      case Disjunction(ds: List[Sentence]) => {

        val t = ds.map(_.traverseFilter(b, newPath, noInAntecedents, findMaxOne))

        val clt = t

        val cl: Vector[(Sentence, Vector[Sentence], Int)] = clt.reduce(_ ++ _)

        cl

      }

      case Conjunction(ds: List[Sentence]) => {

        val t = ds.map(_.traverseFilter(b, newPath, noInAntecedents, findMaxOne))

        val clt = t //(clt, post, negt) = t.unzip3

        val cl: Vector[(Sentence, Vector[Sentence], Int)] = clt.reduce(_ ++ _)

        cl

      }

      case UniversalQuantifier(variable: Variable, sentence: Sentence, _) => {

        val t = sentence.traverseFilter(b, newPath, noInAntecedents, findMaxOne)

        val cl: Vector[(Sentence, Vector[Sentence], Int)] = t

        cl

      }

      case ExistentialQuantifier(variable: Variable, sentence: Sentence/*, _*/) => {

        val t = sentence.traverseFilter(b, newPath, noInAntecedents, findMaxOne)

        val cl: Vector[(Sentence, Vector[Sentence], Int)] = t

        cl

      }

    }

    val clr = tb ++ cltt

    clr

  }

  def replace(old: Sentence, newS: Sentence)

  def deepClone: Sentence

  def forLP(allownotnotliterally: Boolean = true): String

}
