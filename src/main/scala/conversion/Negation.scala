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

case class Negation(var sentence: Sentence) extends Sentence {

  override def equals(that: Any): Boolean = that match {

      case x: Negation => x.sentence == this.sentence

      case _ => false

    }

  override def toString: String = "(not (" + sentence + "))"

  override def replace(old: Sentence, newS: Sentence): Unit = {
    // note that we replace by reference, not by value

    if (sentence.eq(old))
      sentence = newS

  }

  override def deepClone = Negation(sentence.deepClone)

  override def forLP(allownotnotliterally: Boolean): String = {

    sentence match {

      case Negation(Negation(Negation(Negation(ss)))) => {

        "not " + ss.forLP(allownotnotliterally) // not not not not not = not not not = not

      }

      case Negation(Negation(Negation(ss))) if !allownotnotliterally => {

        "{ not " + ss.forLP(allownotnotliterally) + "}0" // see below (not not not not ss = not not ss)

      }

      case Negation(Negation(ss)) => {

        "not " + ss.forLP(allownotnotliterally) // not not not = not

      }

      case Negation(s) if !allownotnotliterally => {

        "{ not " + s.forLP(allownotnotliterally) + "}0" // required for Gringo3/Lparse.
        // Note (1): we might get nested aggregates this way which must be handled later if these are not allowed
        // by ASP grounder.
        // Note (2): As we are in ASP, not not s != s. Instead not not s is strongly equiv to {not s}0

      }

      case _ => "not " + sentence.forLP(allownotnotliterally)

    }

  }

}
