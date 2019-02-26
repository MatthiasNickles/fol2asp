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

case class Disjunction(var ds: List[Sentence]) extends Sentence {

  assert(ds.length == 2)  // for > 3 we use nested disjunctions, in order to simplify rule transformations

  override def toString: String = "(" + ds.map(d => if (d.isInstanceOf[Predicate]) d.toString else "(" + d.toString + ")").reduceLeft(_ + " | " + _) + ")"

  override def replace(old: Sentence, newS: Sentence): Unit = {

    ds = ds.map(sentence => if (sentence.eq(old))
      newS
    else
      sentence)

  }

  override def deepClone = Disjunction(ds.map(_.deepClone))

  override def forLP(allownotnotliterally: Boolean): String = "<error>"

}
