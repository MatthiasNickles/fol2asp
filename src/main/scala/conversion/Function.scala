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

case class Function(symbol: String, as: List[FOLTerm]) extends FOLTerm {

  val args = as //List(as:_*)

  override def equals(that: Any): Boolean = that match {

      case x: Function => x.symbol == this.symbol && x.args == this.args

      case _ => false

    }

  override def toString = if (args.isEmpty)
      symbol
    else
      symbol + "(" + args.map(_.toString).reduceLeft(_ + "," + _) + ")"

  override def collectVars: Vector[Variable] = as.map(_.collectVars).reduce(_ ++ _)

  override def replaceVar(oldVarSymbol: String, newVarSymbol: String): FOLTerm = Function(symbol, as.map(_.replaceVar(oldVarSymbol, newVarSymbol)))

}
