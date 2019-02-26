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

import commandline.fol2asp
import conversion.Converter.{Fol2aspConfig, generateNewPredName}

import scala.collection.mutable.ArrayBuffer

/**
  * @author Matthias Nickles
  *
  */
object HWFOLParser {

  var allowAggregates = true  // we don't examine or amend these though

  def getTopLevelOp(ff: String, op: String /*max recognized op length is 2*/): (Int /* index of first op occurrence (of op's first character) within ._2 or -1.
  Index relative to context of ._2 -->*/ , String) = {

    val f = ff.trim

    var i = 0

    val fl = f.length

    var bStack = List[Int]()

    var outerBrackets = false  // redundant top-level brackets?

    while (i < fl) {

      val c = f(i)

      if (c == '(') {

        bStack = bStack.+:(i)

      } else if (c == ')') {

        if (bStack.isEmpty) throw HWParseException("Unmatched bracket in " + f + ": " + f.substring(i, (i + 10).min(fl)) + "...")
        else {

          if (i == fl - 1 && bStack.length == 1 && bStack.head == 0)
            outerBrackets = true

          bStack = bStack.tail

        }

      } else if (c == op.charAt(0) && bStack.isEmpty) {

        if (op.length == 1)
          return (i, f)
        else if (i < fl - 1 && f(i + 1) == op.charAt(1) && (i == 0 || !c.isLetterOrDigit || !f(i - 1).isLetterOrDigit))
          return (i, f)

      }

      i += 1

    }

    if (!bStack.isEmpty)
      throw HWParseException("Unmatched bracket in " + f + ": " + f.substring(i, (i + 10).min(fl)) + "...")

    if (outerBrackets)
      getTopLevelOp(f.substring(1, fl - 1), op)
    else
      (-1, f)

  }

  def parseTerm(f: String): FOLTerm = {

    val fs = f.dropWhile(_ == '_')

    if (!fs.isEmpty && fs(0).isUpper)
      Variable(f)
    else if (!fs.contains('('))
      Constant(f)
    else {

      val pf = parsePredicateOrFunction(f)

      Function(pf._1, pf._2)

    }
  }

  def parseArgs(as: String): List[String] = {

    val (commaIndex, fa) = getTopLevelOp(as, ",")

    if (commaIndex < 0)
      List(fa)
    else {

      val a1 = fa.substring(0, commaIndex).trim

      val ar = fa.substring(commaIndex + 1)

      a1 :: parseArgs(ar)

    }

  }

  @inline def parseArgsAsTerms(as: String): List[FOLTerm] = parseArgs(as).map(parseTerm(_))

  @inline def isNonStandardPred(fb: String): Boolean = {

    fb.length > 0 && !fb(0).isLower && fb(0) != '_' && fb(0) != '-'

  }

  def parsePredicateOrFunction(f: String): (String, List[FOLTerm]) = {

    if (isNonStandardPred(f))
      (f, Nil)
    else {

      if (!f.contains('(')) {

        (f, Nil)

      } else {

        val fn = f.takeWhile(_ != '(')

        val fs = f.substring(fn.length).drop(1).trim.stripSuffix(")").trim

        val args = parseArgsAsTerms(fs)

        (fn.trim, args)

      }

    }

  }

  def parseQuantifier(fq: String, additionalTopLevelSentences: ArrayBuffer[Sentence], config: Fol2aspConfig): Sentence /*conjunction of quantifiers */ = {

    val hd = fq.takeWhile(_ != ':')

    val sStr = fq.substring(hd.length + 1)

    val params = hd.dropWhile(_ != ' ').trim

    val s: Sentence = parseI(sStr, additionalTopLevelSentences, config)

    val varsAndDPsStrs = parseArgs(params)

    val (vars, dps) = varsAndDPsStrs.partition { x => {

      val y = x.dropWhile(_ == '_')

      y.length > 0 && y(0).isUpper

    }

    }

    genQuants(vars = vars,
      domainPreds = dps.map(parseI(_, additionalTopLevelSentences, config)),
      body = s,
      isUni = fq.charAt(0) == 'F' /*-ORALL*/)

  }

  @inline def createN_aryConjunction(sentences: List[Sentence]) = {

    assert(sentences.length >= 1)

    if(sentences.length == 1)
      sentences.head
    else {

      if (sentences.length == 2)
        new Conjunction(sentences)
      else
        sentences.reduce((s1: Sentence, s2: Sentence) => Conjunction(List(s1, s2)))

    }

  }

  def genQuants(vars: List[String],
                domainPreds /*those inline after FORALL or EXIST. Also all (preceeding) #domain bindings have already been stored into domainPreds at this point*/ : List[Sentence],
                body: Sentence,
                isUni: Boolean /*true: FORALL, false: EXIST*/): Sentence = {

    val r = vars.map(x => Variable(x)).foldRight(body: Sentence) {
      case (v: Variable, sb: Sentence) => {

        val matchingDomainPreds: Seq[Sentence] = domainPreds.filter { case Predicate(sym, as) => as.exists { case asv: Variable => asv.symbol == v.symbol } }.distinct

        val varBindingPred = if (matchingDomainPreds.isEmpty) {

          fol2asp.stomp(-302, v.symbol.toString + " in " + body)

          None

        } else {

          if (matchingDomainPreds.length > 1)
            fol2asp.stomp(-303, v.symbol.toString + ": " + matchingDomainPreds.mkString(", ") + (if(isUni) "; using only the first of these!" else ""))

          Some(matchingDomainPreds.head.asInstanceOf[Predicate]) // for universal quantifiers, we allow max one variable binding in the quantifier

        }

        if (isUni)
          UniversalQuantifier.apply(v, sb, varBindingPred)
        else // to create an ExistentialQuantifier, we don't need to retain the domain bindings in the quantifier as we can simply move them
          // into the body using conjunction:
          ExistentialQuantifier.apply(v, /*new Conjunction*/ createN_aryConjunction(sb :: matchingDomainPreds.toList /*we don't need to restrict ourselves to varBindingPred here*/))

      }

    }

    fol2asp.log("Generated quantifier: " + r)

    r

  }

  /**
    * Simple top-down parser. Should be fast enough for typical lengths of manually-created formulas, perhaps not so fast for large encodings (more tests required).
    * Not a general-purpose FOL parser; designed for use within project fol2asp only.
    *
    * @author Matthias Nickles
    *
    * @param ff
    * @param additionalTopLevelSentences
    * @param config
    * @return
    */
  def parseI(ff: String, additionalTopLevelSentences: ArrayBuffer[Sentence], config: Fol2aspConfig): Sentence = {

    val r = {

      val f = ff.trim

      fol2asp.log("Parsing subformula " + f + "...")

      val (uniquantIndex, fqa) = getTopLevelOp(f, "FORALL")

      if (uniquantIndex >= 0) {

        if (uniquantIndex == 0)
          parseQuantifier(fqa, additionalTopLevelSentences, config)
        else { // quantifier needs to be brought in parentheses if not at start of formula.

          val fx = fqa.substring(0, uniquantIndex - 1) + "(" + fqa.substring(uniquantIndex) + ")"

          parseI(fx, additionalTopLevelSentences, config)

        }

      } else {

        val (exiquantIndex, fqb) = getTopLevelOp(fqa, "EXIST")

        if (exiquantIndex >= 0) {

          if (exiquantIndex == 0)
            parseQuantifier(fqb, additionalTopLevelSentences, config)
          else { // quantifier needs to be brought in parentheses if not at start of formula.

            val fx = fqb.substring(0, exiquantIndex - 1) + "(" + fqb.substring(exiquantIndex) + ")"

            parseI(fx, additionalTopLevelSentences, config)

          }

        } else {

          val (equivIndex, fca) = getTopLevelOp(fqb, "\u00A7") // <->

          if (equivIndex >= 0) {

            val a1 = fca.substring(0, equivIndex)

            val a2 = fca.substring(equivIndex + 1)

            val s1 = parseI(a1, additionalTopLevelSentences, config)

            val s2 = parseI(a2, additionalTopLevelSentences, config)

            Conjunction(Conditional(s1, s2) :: Conditional(s2, s1) :: Nil)

          } else {

            val (convCondIndex, fcb) = getTopLevelOp(fca, "<-")

            if (convCondIndex >= 0) {

              val a1 = fcb.substring(0, convCondIndex)

              val a2 = fcb.substring(convCondIndex + 2)

              ConvConditional(parseI(a1, additionalTopLevelSentences, config), parseI(a2, additionalTopLevelSentences, config))

            } else {

              val (condIndex, fcc) = getTopLevelOp(fcb, "->")

              if (condIndex >= 1) {

                val a1 = fcc.substring(0, condIndex)

                val a2 = fcc.substring(condIndex + 2)

                Conditional(parseI(a1, additionalTopLevelSentences, config), parseI(a2, additionalTopLevelSentences, config))

              } else {

                val (disjIndex, fa) = getTopLevelOp(fcc, "|")

                if (disjIndex >= 0) {

                  val a1 = fa.substring(0, disjIndex)

                  val a2 = fa.substring(disjIndex + 1)

                  Disjunction(parseI(a1, additionalTopLevelSentences, config) :: parseI(a2, additionalTopLevelSentences, config) :: Nil)

                } else {

                  val (conjIndex, fb) = getTopLevelOp(fa, "&")

                  if (conjIndex >= 0) {

                    val a1 = fb.substring(0, conjIndex)

                    val a2 = fb.substring(conjIndex + 1)

                    Conjunction(parseI(a1, additionalTopLevelSentences, config) :: parseI(a2, additionalTopLevelSentences, config) :: Nil)

                  } else {

                    if (fb.startsWith("not ") || fb.startsWith("not(") || fb.startsWith("not\t")) {

                      generateDefaultNegation(additionalTopLevelSentences, config, fb.stripPrefix("not"))

                    } else if (fb.startsWith("!")) {

                      if (config.strongexcl)
                        generateStrongNegation(additionalTopLevelSentences, config, fb.drop(1).trim)
                      else
                        generateDefaultNegation(additionalTopLevelSentences, config, fb.drop(1).trim)

                    } else if (fb.startsWith("-") && fb.drop(1).dropWhile(_.isWhitespace).take(1) == "(") { // we allow strong negation also for entire subformulas

                      generateStrongNegation(additionalTopLevelSentences, config, bodyOfClassNegStr = fb.drop(1).trim.drop(1).dropRight(1))

                    } else {

                      if (fb.isEmpty || (!allowAggregates && isNonStandardPred(fb)))
                        throw HWParseException("Invalid or empty predicate '" + fb + "'")

                      val p = parsePredicateOrFunction(fb)

                      Predicate(p._1, p._2)

                    }

                  }

                }

              }

            }

          }

        }

      }

    }

    if (fol2asp.debug)
      println("subformula parse result: " + r)

    r

  }

  @inline def generateStrongNegation(additionalTopLevelSentences: ArrayBuffer[Sentence], config: Fol2aspConfig, bodyOfClassNegStr: String) = {

    val bodyOfClassNeg = parseI(bodyOfClassNegStr, additionalTopLevelSentences, config)

    val nPredStr = generateNewPredName(config)

    val newPred = _root_.conversion.Predicate(nPredStr, Nil)

    val eqDef = Conjunction(Conditional(newPred, bodyOfClassNeg) :: Conditional(bodyOfClassNeg, newPred) :: Nil)

    additionalTopLevelSentences.append(eqDef)

    Predicate("-" + nPredStr, Nil)

  }

  @inline def generateDefaultNegation(additionalTopLevelSentences: ArrayBuffer[Sentence], config: Fol2aspConfig, bodyOfDefaultNeg: String) =
    Negation(parseI(bodyOfDefaultNeg, additionalTopLevelSentences, config))

  def parse(ff: String, config: Fol2aspConfig): Option[Sentence] = {

    val r = try {

      val additionalTopLevelSentences = ArrayBuffer[Sentence]()

      val parsedSentence: Sentence = parseI(ff.replaceAllLiterally("<->", "\u00A7"), additionalTopLevelSentences, config)

      if (additionalTopLevelSentences.isEmpty)
        parsedSentence
      else
        createN_aryConjunction(additionalTopLevelSentences.toList ++ List(parsedSentence))

    } catch {

      case e: HWParseException => {

        fol2asp.stomp(-300, e.s)

        throw e

        return None

      }

    }

    Some(r)

  }

}

case class HWParseException(s: String) extends Exception
