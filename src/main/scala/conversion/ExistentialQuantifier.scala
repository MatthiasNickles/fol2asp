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

case class ExistentialQuantifier(v: Variable, var s: Sentence /*, var ds: Option[Predicate] <- for existential quantifiers, we include
 the domain atoms in the body already during parsing*/) extends Quantifier(v, s, None/*ds*/) {

  override def equals(that: Any): Boolean =
    that match {

      case x: ExistentialQuantifier => x.variable == this.variable && x.body == this.body

      case _ => false

    }

  override def toString: String = "(EXIST " + variable /*+ (if (ds.isEmpty) "" else ", " + ds.mkString(","))*/ + ": (" + body + "))"

  override def replace(old: Sentence, newS: Sentence): Unit = {
    // note that we replace by reference, not by value

    if (s.eq(old))
      s = newS

  }

  override def deepClone = ExistentialQuantifier(variable, body.deepClone/*, domain*/)

  override def forLP(allownotnotliterally: Boolean): String = "<error>"

}
