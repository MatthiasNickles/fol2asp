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

case class Predicate(val symbol: String, var as: List[FOLTerm]) extends AtomicSentence {

  override def equals(that: Any): Boolean = that match {

      case x: Predicate => x.symbol == this.symbol && x.as == this.as

      case _ => false

    }

  override def toString: String =
    if (as == Nil)
      symbol
    else
      symbol + "(" + as.map(_.toString).reduceLeft(_ + "," + _) + ")"

  def replaceVar(oldVarSymbol: String, newVarSymbol: String) = {

    as = as.map(_.replaceVar(oldVarSymbol, newVarSymbol))

  }

  override def replace(old: Sentence, newS: Sentence): Unit = {}

  override def deepClone = Predicate(symbol, as)

  override def forLP(allownotnotliterally: Boolean): String = toString

}
