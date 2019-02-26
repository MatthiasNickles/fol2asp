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

case class Conditional(var premise: Sentence, var conclusion: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {

      case x: Conditional => x.premise == this.premise && x.conclusion == this.conclusion

      case _ => false

    }

  override def toString = "(" + (if (premise.isInstanceOf[Predicate]) premise else "(" + premise + ")") + " -> (" + (if (conclusion.isInstanceOf[Predicate]) conclusion else "(" + conclusion + ")") + "))"

  override def replace(old: Sentence, newS: Sentence): Unit = {
    // note that we replace by reference, not by value

    if (premise.eq(old))
      premise = newS

    if (conclusion.eq(old))
      conclusion = newS
  }

  override def deepClone = Conditional(premise.deepClone, conclusion.deepClone)

  override def forLP(allownotnotliterally: Boolean): String = "<error>"

}
