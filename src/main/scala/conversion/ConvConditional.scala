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

case class ConvConditional(var conclusion: Sentence, var condition: Sentence) extends Sentence {

  override def equals(that: Any): Boolean =
    that match {

      case x: ConvConditional => x.condition == this.condition && x.conclusion == this.conclusion

      case _ => false

    }

  override def toString = (if (conclusion.isInstanceOf[Predicate]) conclusion else "(" + conclusion + ")") + " <- " + (if (condition.isInstanceOf[Predicate]) condition else "(" + condition + ")")

  override def replace(old: Sentence, newS: Sentence): Unit = {
    // note that we replace by reference, not by value

    if (conclusion.eq(old))
      conclusion = newS

    if (condition.eq(old))
      condition = newS

  }

  override def deepClone = ConvConditional(conclusion.deepClone, condition.deepClone)

  override def forLP(allownotnotliterally: Boolean): String = "<error>"

}
