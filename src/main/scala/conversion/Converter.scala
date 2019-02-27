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

import java.util.regex.Pattern

import commandline.fol2asp

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/**
  *  Converter uses FOL->ASP formula transformation rules (note a few minor points where there is room for interpretation or ambiguity, see fol2asp source code) published in
  *   [1] Joohyung Lee, Ravi Palla (‎2009): "Reformulating the Situation Calculus and the Event Calculus in the General
  *       Theory of Stable Models and in Answer Set Programming". In JAIR Vol. 43.
  *   [2] Joohyung Lee, Ravi Palla (‎2009): "System F2LP - Computing Answer Sets of First-Order Formulas". In Procs. LPNMR 2009.
  *   [3] Pedro Cabalar, David Pearce, Agustin Valverde (‎2005): "Reducing Propositional Theories in Equilibrium Logic to Logic Programs". In Procs. EPIA 2005. (for
  *   NNF, rules L1-L5, R1-R5)
  *   [3] references Vladimir Lifschitz, Lappoon R. Tang, Hudson Turner: "Nested Expressions in Logic Programs" (In Annals of Mathematics and Artificial Intelligence 1999) for some of the transformation rules.
  *
  * @author Matthias Nickles
  */
object Converter {

  var newVarCounter: java.util.concurrent.atomic.AtomicLong = new java.util.concurrent.atomic.AtomicLong(1l)

  var newPredCounter: java.util.concurrent.atomic.AtomicLong = new java.util.concurrent.atomic.AtomicLong(1l)

  var genTrueFalseRules = false

  @inline def mightBeFOL(f: String): Boolean = {

    !f.contains(":-") /*<- TODO: can we ignore combinations of conditions with classic negation, e.g., c:-a */ &&
      ((f.contains("&") || f.contains("FORALL ") || f.contains("FORALL\t") || f.contains("EXIST ") || f.contains("EXIST\t")
        || f.contains("->") || f.contains("<-") || f.contains("|") || f.trim.startsWith("not ") || f.trim.startsWith("not\t")
        || f.trim.startsWith("(")))

  }

  @inline def mightBeMLN(f: String): Boolean = {

    f.contains("=>") || /*f.contains("!") ||*/
      f.contains(" v ") || f.contains("\tv ") || f.contains(" v\t") || f.contains("\tv\t") ||
      f.contains("^") || (f.startsWith("EXIST ") || f.startsWith("EXIST\t")) && !f.contains(":")

  }

  /**
    * Converts MLN-style rule or clause into our standard FOL format
    *
    * @author Matthias Nickles
    *
    * @param f
    * @return standard FOL formula
    */
  def convertMLNRule(f: String): String = {

    if (fol2asp.debug)
      println("Preprocessing MLN-style rule: " + f)

    var nestingLevel = 0

    var insertColon = f.startsWith("EXIST")

    var expectAnotherVariable = insertColon

    val newF = new mutable.StringBuilder()

    var i = if (insertColon) {

      newF.append("EXIST")

      5

    } else 0

    while (i < f.length) {

      val c = f(i)

      if (c == '(')
        nestingLevel += 1
      else if (c == ')')
        nestingLevel -= 1

      if (insertColon && c == ',') {

        expectAnotherVariable = true

      } else if (c.isLetterOrDigit || c == '_' || c == '!')
        expectAnotherVariable = false

      if (nestingLevel == 0 && c == 'v' && i > 0 && f(i - 1).isWhitespace && i < f.length - 1 && f(i + 1).isWhitespace)
        newF.append(" | ")
      else if (nestingLevel == 0 && (c == '^' || c == ',' && !insertColon))
        newF.append(" & ")
      else if (nestingLevel == 0 && c == '=' && i < f.length - 1 && f(i + 1) == '>') {

        newF.append(" -> ")

        i += 1

      } else if (insertColon && c.isWhitespace && !expectAnotherVariable) {

        insertColon = false

        newF.append(": ")

      } else
        newF.append(c)

      i += 1

    }

    val r = newF.toString.trim.stripSuffix(".") + "."

    if (fol2asp.debug)
      println("Result: " + r)

    r

  }

  @inline def isMeta(f: String, exceptHide: Boolean = false): Boolean = {

    val formula = f.trim

    formula.startsWith("#hide") || formula.startsWith("#show") || formula.startsWith("#const") || formula.startsWith("#domain") ||
      formula.startsWith("#external") || formula.startsWith("#program") || formula.startsWith("#script") || formula.startsWith("#include") ||
      formula.startsWith("#begin_lua") || formula.startsWith("#end") || formula.startsWith("#compute") ||
      formula.startsWith("#base") || formula.startsWith("#cumulative") || formula.startsWith("#volatile") ||
      formula.startsWith("#minimize") || formula.startsWith("#maximize")
    // #minimize and #maximize are strictly speaking not meta-statements, but they are also not logical formulas

  }

  /**
    * Translates a FOL theory (under stable model semantics) into ASP syntax. Besides standard FOL formulas, also ASP, MLN-style rules and certain #meta statements
    * are allowed (see documentation). Note that only those parts of the input necessary for FOL->ASP translation purposes are parsed - any other input is copied
    * to the output unchecked.
    *
    * @author Matthias Nickles
    *
    * @param folStrRR
    * @param verbose
    * @param refeedD7b
    * @param moduleIndex
    * @param notnotLiterallyAllowed
    * @param disjSymbol
    * @param prefixnp
    * @param prefixnv
    * @param retainfol
    * @param mlnrules
    * @param omitcomments
    * @param strongexcl
    * @param omitDomainDecls
    * @return ASP encoding
    */
  def fol2aspProg(folStrRR: String, verbose: Boolean, refeedD7b: Boolean,
                  moduleIndex: Long /* if true, we append this to the names for new variables and predicates (for concurrent uses).
                   To be used alternatively to prefixnp and prefixnv */ ,
                  notnotLiterallyAllowed: Boolean,
                  disjSymbol: String,
                  prefixnp: String,
                  prefixnv: String,
                  retainfol: Boolean,
                  mlnrules: Boolean,
                  omitcomments: Boolean,
                  strongexcl: Boolean,
                  omitDomainDecls: Boolean): String = {

    val stringLiterals = ArrayBuffer[String]()

    val comments = mutable.HashMap[Int, ArrayBuffer[String]]()

    var commentMaxFormulaNo = 0

    val scriptReplacement = folStrRR.indexOf("#end_lua").max(folStrRR.indexOf("#end."))

    val folStrR = if (scriptReplacement >= 0) folStrRR.drop(scriptReplacement).dropWhile(_ != '.').drop(1) else folStrRR
    // ^ otherwise, we wouldn't handle % in scripts properly

    val folStr = {

      // TODO: optimize this (e.g., substitute all regular expressions)

      val stringLitPat = "'([^\\\\']+|\\\\([btnfr\"'\\\\]|[0-3]?[0-7]{1,2}|u[0-9a-fA-F]{4}))*'|\"([^\\\\\"]+|\\\\([btnfr\"'\\\\]|[0-3]?[0-7]{1,2}|u[0-9a-fA-F]{4}))*\"".r

      var folStrS = stringLitPat.replaceAllIn(folStrR, _ match {

        case c: Regex.Match => {

          stringLiterals.append(c.toString)

          fol2asp.strLitPrefix + (stringLiterals.length - 1) + "_"

        }

      })

      val commentPat1 = "(?s)(%\\*.*?\\*%)".r  // we can't put all comment patterns into the same regex because (?s) apparently stops working then (?)

      val commentPat2 = "%.*".r

      val commentPat3 = "(?s)/\\*.*?\\*/".r

      val commentPat4 = "//.*".r

      def replaceComment(c: Regex.Match) = {

        if (!omitcomments) {

          val lineNo = c.before.toString.count(_ == '.') // roughly the position where we will reinsert the comment (not accurate)

          val commentsInLine = comments.getOrElseUpdate(lineNo, ArrayBuffer[String]())

          val cStr = c.toString.trim

          val cStrASP = if (cStr.startsWith("//"))
            "%" + cStr.drop(2)
          else if (cStr.startsWith("/*"))
            "%*" + cStr.drop(2).dropRight(2) + "*%"
          else
            cStr

          commentsInLine.append(cStrASP)

          comments.put(lineNo, commentsInLine)

          commentMaxFormulaNo = commentMaxFormulaNo.max(lineNo)

        }

        ""

      }

      val r2 = commentPat1.replaceAllIn(folStrS, _ match {

        case c: Regex.Match => replaceComment(c)

      })

      val r3 = commentPat2.replaceAllIn(r2, _ match {

        case c: Regex.Match => replaceComment(c)

      })

      val r4 = commentPat3.replaceAllIn(r3, _ match {

        case c: Regex.Match => replaceComment(c)

      })

      val r5 = commentPat4.replaceAllIn(r4, _ match {

        case c: Regex.Match => replaceComment(c)

      })

      r5

    }

    val domainDecls = new StringBuilder()

    val ls: Iterator[String] = folStr.lines

    val config = Fol2aspConfig(refeedD7b = refeedD7b,
      nnE = false,
      r5dash = false,
      moduleIndex = moduleIndex,
      notnotInD7b = true /*TODO: funny results with notnotInD7b = false (check again)*/ ,
      notnotLiterallyAllowed = notnotLiterallyAllowed,
      disjSymbol = disjSymbol,
      prefixnp = prefixnp,
      prefixnv = prefixnv,
      strongexcl = strongexcl)

    var formulaNo = 0

    val rr: Seq[String] = ls.foldLeft((Vector[String](), false)) {
      case ((bR, inScriptR), lR) => {

        val line = lR.trim

        formulaNo += 1  // we currently only use this to reinsert removed comments

        val conversionResult = if (line.isEmpty)
          (bR, inScriptR)
        else {

          val inScript = if (line.startsWith("#begin_lua") || line.startsWith("#script")) true else if (line.startsWith("#end_lua") || line.startsWith("#end.")) false else inScriptR

          val newL = if (inScript) line
          else {

            if (line.startsWith("#domain")) {
              // We perform just one pass, so #domain needs to be located before any quantifier which uses it. We
              // (in translateFOLFormula) copy the #domain atoms directly behind FORALL / EXIST.
              // (Note that #domain is deprecated, so better avoid it altogether in input.)

              val stripped = line.substring(7).trim.stripSuffix(".").trim

              val predName = stripped.takeWhile(_ != '(')

              val varsInDomainSpec = stripped.dropWhile(_ != '(').drop(1).dropRight(1).split(';').map(_.trim)

              varsInDomainSpec.foreach(varInDomainSpec => domainDecls.append("," + predName + "(" + varInDomainSpec + ")"))

              if (omitDomainDecls)
                "% " + line
              else
                line

            } else if (mlnrules && mightBeMLN(line)) {

              val folFormStr = convertMLNRule(line)

              println("% Translated MLN-style rule to " + folFormStr)

              (if (retainfol) "% " + line + "\n" else "") + translateFOLFormula(verbose, domainDecls, config, folFormStr)

            } else if (mightBeFOL(line) && !isMeta(line)) {

              (if (retainfol) "% " + line + "\n" else "") + translateFOLFormula(verbose, domainDecls, config, line)

            } else
              line

          }

          (bR.:+(newL), inScript)

        }

        (conversionResult._1 ++ comments.getOrElse(formulaNo - 1, ArrayBuffer[String]()), conversionResult._2)

      }

    }._1

    val rrs = rr.mkString("\n")

    val rr3: String = rrs + (if (genTrueFalseRules) "\ntrue.\n:- false." else "") +
      "\n" + comments.filter(_._1 > formulaNo - 1).values.map(_.mkString("\n")).mkString("\n") // because the formula index in comments is not fully accurate, we might have missed a few comments which we append now

    val strLitRPat = (fol2asp.strLitPrefix + "(\\d+)_").r

    val rr4 = strLitRPat.replaceAllIn(rr3, _ match { case strLitRPat(index) => Regex.quoteReplacement(stringLiterals(index.toInt)) }) // without quoteReplacement replaceAllIn silently drops backslashes (Java behavior)

    val rss = if (scriptReplacement >= 0) {

      val script = folStrRR.take(folStrRR.indexOf(".", scriptReplacement) + 1)

      script + "\n\n" + rr4

    } else
      rr4

    if (fol2asp.debug)
      println("Final result of fol2asp:\n" + rss)

    rss

  }

  /**
    * Translates a single FOL formula to an ASP encoding (which might comprise several rules).
    * MLN-style formulas need to be preprocessed before calling this function (see translateMLNRule()).
    *
    * @author Matthias Nickles
    *
    * @param verbose
    * @param domainDecls
    * @param config
    * @param line
    * @return ASP encoding
    */
  def translateFOLFormula(verbose: Boolean, domainDecls: StringBuilder, config: Fol2aspConfig, line: String): String = {

    val varListInQuantifPattern = "(?<=(\\b(FORALL|EXIST)\\s))[^\\:]+\\:".r

    val ln = varListInQuantifPattern.replaceAllIn(line, _ match {

      case vlm: Regex.Match => {

        val domainDeclsStr = domainDecls.toString.trim.stripPrefix(",").trim

        val varListStr = vlm.toString.stripSuffix(":").trim

        if (!domainDeclsStr.isEmpty && !varListStr.isEmpty)
          varListStr + " , " + domainDeclsStr + ":"
        else
          varListStr + domainDeclsStr + ":"

      }

    }
    )

    val r: Option[String] = fol2aspSingleMemo((Left(ln), verbose, config))

    r.getOrElse(line)

  }

  var domainPredBindings = Vector[(String, Predicate)]() // since fol2aspSingle might call itself, we need a global variable to keep
  // track of domain predicate binding (these will later be added to rule bodies, as a subsitute for #domain decls which don't exist in Gringo 4 and Gringo 5)
  // Works only because we always(!) replace quantifier variables with globally new variables!

  def depthstr(depth: Int) = "                                                                                                                     ".take(depth * 3)

  case class Fol2aspConfig(refeedD7b: Boolean = false,
                           nnE: Boolean = false,
                           r5dash: Boolean = true /* to get an effect similar as with using tool F2LP use "false" here, but this could lead to more formulas*/ ,
                           moduleIndex: Long /* if true, we append this to the names for new variables and predicates (for concurrent uses).
                            To be used alternatively to prefixnp and prefixnv */ ,
                           notnotInD7b: Boolean /* Def 1(b) in [2] and Def 7(b) in [1] seem to differ here. */ ,
                           notnotLiterallyAllowed: Boolean /* not related to above; means we can move negative atoms in heads into body as ... :- not not l (Gringo 4 and higher), ... instead of ... :- {not l}0, ... (Gringo 3, 4 and higher).
                           The former is understood by Gringo/Clingo >=4 only but has the advantage that aggregates aren't nested if l is an aggregate*/ ,
                           disjSymbol: String,
                           prefixnp: String,
                           prefixnv: String,
                           strongexcl: Boolean) {}

  /**
    * Main FOL->ASP conversion procedure for single formulas. Typically not called directly (use translateFOLFormula() instead).
    *
    * @author Matthias Nickles
    *
    * @param folStr1E
    * @param verbose
    * @param config
    * @return Some(ASP encoding) or None
    */
  def fol2aspSingle(folStr1E: Either[String, Sentence], verbose: Boolean, config: Fol2aspConfig): Option[String] = {

    def parseFOL(in: String): Option[Sentence] = {
      try {

        {

          val inStr = in.trim.stripSuffix(".").trim

          val parseResult = {
            try {

              HWFOLParser.parse(inStr, config)

            } catch {

              case e: Exception => {

                fol2asp.stomp(-300, "Cannot parse " + inStr + " as FOL encoding: " + e)

                if (fol2asp.debug)
                  e.printStackTrace

                None

              }

            }

          }

          if (fol2asp.debug)
            println("HW FOL parser result: " + parseResult + "\n")

          parseResult

        }

      } catch {

        case x: Exception => {

          if (verbose || fol2asp.debug)
            println("INFO: Couldn't parse " + in + " as FOL formula: " + x)

          return None

        }

      }

    }

    var fol1: Sentence = folStr1E.fold(ss => parseFOL(ss) getOrElse {
      return None
    }, sent => sent)

    fol2asp.log("Original FOL formula (parsed): " + fol1)

    def findMaxOccQuant(fTop: Sentence): Option[Vector[(Sentence /*discovered quantifier branch*/ , Vector[Sentence] /*path to branch*/ ,
      Int /*number of times branch occurs in antecedent of implication or in negation*/ )]] = {

      val maxQuants: (Vector[(Sentence, Vector[Sentence], Int)]) =
        fTop.traverseFilter(b = {
          case (s, pathToS, _) => {

            s match {

              case value: Quantifier => !pathToS.exists(_.isInstanceOf[Quantifier]) // we look for quantifiers which are not subformulas of any other quantifier

              case _ => false

            }

          }
        }, path = Vector[Sentence](), noInAntecedents = 0, findMaxOne = true
        )

      if (maxQuants.isEmpty)
        None
      else {

        Some(maxQuants)

      }

    }

    if (config.nnE) {

      // we replace all strictly positive maximal occurrences of existential quantifiers E with not not E:

      val maxExQuantsStrictPos: (Vector[(Sentence, Vector[Sentence], Int)]) =
        fol1.traverseFilter(b = {
          case (s, pathToS, noInAntec) => {

            s match {

              case value: Quantifier => noInAntec == 0 && !pathToS.exists(_.isInstanceOf[ExistentialQuantifier]) // we look for quantifiers which are not subformulas of any other quantifier

              case _ => false

            }

          }
        }, path = Vector[Sentence](), noInAntecedents = 0, findMaxOne = false
        )

      maxExQuantsStrictPos.foreach {
        case (quant, path, _) => {
          // replace nested such quantifiers

          if (!path.isEmpty) {

            val parent = path.last

            parent.replace(quant, _root_.conversion.Negation(_root_.conversion.Negation(quant)))

            fol1

          }

        }
      }

      // also replace top-level such quantifier:

      if (!maxExQuantsStrictPos.isEmpty && maxExQuantsStrictPos(0)._1.eq(fol1))
        fol1 = _root_.conversion.Negation(_root_.conversion.Negation(maxExQuantsStrictPos(0)._1))

      if (fol2asp.debug)
        println("Formula after adding not not as prefix to strictly positive existential quantifiers")

    }

    var xOpt: Option[Vector[(Sentence, Vector[Sentence], Int)]] = None

    while ( {
      xOpt = findMaxOccQuant(fol1);
      xOpt
    }.isDefined) {
      // we apply Def. 7 in [1]

      val x: Vector[(Sentence, Vector[Sentence], Int)] = xOpt.get

      val subFol1 = x.distinct

      val theOneWithPath: (Quantifier, Vector[Sentence] /* the path*/ , Int) = x(0).asInstanceOf[(Quantifier, Vector[Sentence], Int)]

      val isXPos = theOneWithPath._3 % 2 == 0

      val isXNeg = !isXPos

      val isXstrictlyPos = theOneWithPath._3 == 0

      if (fol2asp.debug && isXPos) println("Quantifier '" + theOneWithPath._1 + "' is positive in '" + fol1)

      if (fol2asp.debug && isXstrictlyPos) println("Quantifier '" + theOneWithPath._1 + "' is strictly-positive in '" + fol1)

      if (fol2asp.debug && isXNeg) println("Quantifier '" + theOneWithPath._1 + "' is negative in '" + fol1)

      def freeVarsInTheOne(theOne: Quantifier, excludeTheOne: Boolean): Vector[(Variable, Predicate)] = {
        // Important: we obtain a list of (variable, parentPred) pairs, in order to be able later to replace variables

        var collVars = Vector[(Variable, Predicate /*parent of variable*/ )]() // TODO: could inject a monad into traverseFilter

        theOne.traverseFilter(b = {
          // we traverse only in order to gather free variables, we don't collect filtered sentences
          case (s, pathToS, _) => {

            s match {

              case pred: Predicate => {

                var vars: Vector[(Variable, Predicate)] = if (pred.as.isEmpty) Vector[(Variable, Predicate)]() else pred.as.map(_.collectVars).reduce(_ ++ _).map((_, pred))

                val p = if (excludeTheOne) pathToS.filterNot(_.eq(theOne)) else pathToS

                val freeVars = vars.filter {
                  case (value, _) => !p.exists {
                    // is variable bound by any quantifier toward the AST root?

                    case upperS: Quantifier => {

                      upperS.variable.symbol == value.symbol

                    }

                    case _ => false

                  }
                }

                collVars = collVars ++ freeVars

                false

              }

              case _ => false

            }

          }
        }, path = Vector[Sentence]() /*since theOne is maximal, there are no quantifiers further upwards */ , 0, findMaxOne = false
        )

        collVars

      }

      def getDomainPredForQuantWithRenaming(varSymOpt: Option[String]): Option[Predicate] = {

        if (theOneWithPath._1.domain.isEmpty)
          None
        else {

          val addToBody: Predicate = theOneWithPath._1.domain match {

            case Some(Predicate(ps, as)) => Predicate(ps, {
              as.map { case v@Variable(vs: String) => if (vs == theOneWithPath._1.variable.symbol && varSymOpt.isDefined) Variable(varSymOpt.get) else v }
            })

          }

          Some(addToBody)

        }

      }

      def renameQuantifVar(variable: Variable): Unit = {

        val nVarName = config.prefixnv + newVarCounter.getAndAdd(1l) + (if (config.moduleIndex > 0l) "_" + config.moduleIndex else "")

        val freeVarsInclQuantifVar = freeVarsInTheOne(theOneWithPath._1, true)

        freeVarsInclQuantifVar.foreach {
          case (v, parentPred) => {

            if (v.symbol == variable.symbol)
              parentPred.replaceVar(variable.symbol, nVarName)

          }

        }

        // Since this method is called iff a quantifier is removed, we use this place also to collect domainPredBinding for the new variables
        // (these will later be added to the rule body or as #domain statements):

        val domainPredOpt1: Option[Predicate] = getDomainPredForQuantWithRenaming(Some(nVarName))

        val domainPredOpt2: Option[Predicate] = domainPredBindings.find(_._1 == variable.symbol).map(_._2) // we also check whether the original variable name
        // is bound

        if (domainPredOpt1.isDefined && !domainPredOpt2.isDefined)
          domainPredBindings = domainPredBindings.:+((nVarName, domainPredOpt1.get))
        else if (domainPredOpt2.isDefined && !domainPredOpt1.isDefined)
          domainPredBindings = domainPredBindings.:+((nVarName, domainPredOpt2.get))
        else if (domainPredOpt1.isDefined && domainPredOpt2.isDefined)
          fol2asp.stomp(-301, "inconsistent variable domain information in quantifier " + theOneWithPath._1)

      }

      def updateF_A(variable: Variable /* the quantifier variable */): Sentence = {
        // we replace Qx:blub with blub where x in blub becomes a new variable

        renameQuantifVar(variable)

        val replacement = theOneWithPath._1.body
        // ^ domain predicate binding gets mangled later (e.g., might be moved to head of some rule), so we later need to treat the added binding further

        if (!theOneWithPath._2.isEmpty) {

          val parentOfTheOne = theOneWithPath._2.last

          parentOfTheOne.replace(theOneWithPath._1, replacement)

          fol1

        } else
          replacement

      }

      val newFOL = theOneWithPath._1 match {

        // for performance reasons, we mutate the AST in-place:

        case ExistentialQuantifier(variable: Variable, sentence: Sentence) if isXNeg => updateF_A(variable)

        case UniversalQuantifier(variable: Variable, sentence: Sentence, _) if isXPos => updateF_A(variable)

        case ExistentialQuantifier(variable: Variable, sentence: Sentence) if isXPos => {

          val nPredStr = generateNewPredName(config)

          val freeVars = freeVarsInTheOne(theOneWithPath._1, false)

          val newPred = _root_.conversion.Predicate(nPredStr, freeVars.unzip._1.toList.map(t => _root_.conversion.Variable(t.symbol)))

          val replaceInFf = if (config.notnotInD7b) conversion.Negation(conversion.Negation(newPred)) else newPred

          val fol2 = if (!theOneWithPath._2.isEmpty) {

            // note that [1] and [2] appear to differ here? [2] proposes to replace with not not newPred instead of just newPred

            val parentOfTheOne = theOneWithPath._2.last

            parentOfTheOne.replace(theOneWithPath._1, replaceInFf)

            fol1

          } else
            replaceInFf

          renameQuantifVar(variable) // we need a unique variable name so that we can later add unambigious domain predicate bindings

          val sentenceWithVarBinding: Sentence = theOneWithPath._1.body

          val pFol = _root_.conversion.Conditional(sentenceWithVarBinding, newPred)

          if (config.refeedD7b) {

            fol2asp.log("Applying config refeedD7b=true:\n   1) " + fol2 + "\n& 2) " + pFol)

            val r12: Seq[Option[String]] = List(fol2.deepClone, pFol).map(s => fol2aspSingleMemo((Right(s), verbose, config)))

            return Some(r12(0).getOrElse("") + "\n" + r12(1).getOrElse(""))

          } else {

            val fol3 = _root_.conversion.Conjunction(List(fol2, pFol))

            fol3

          }

        }

        case UniversalQuantifier(variable: Variable, sentence: Sentence, domainPredOpt: Option[Predicate]) if isXNeg => {

          val sentenceWithVarBinding: Sentence = theOneWithPath._1.body

          val bodyOfExist: Negation = _root_.conversion.Negation(sentenceWithVarBinding)

          val rp = _root_.conversion.Negation(_root_.conversion.ExistentialQuantifier(v = _root_.conversion.Variable(variable.symbol) /*<- we copy the variable, don't want cycles in AST*/ ,
            s = HWFOLParser.createN_aryConjunction(bodyOfExist :: domainPredOpt.toList) /*, domainPredOpt*/))

          val fol2 = if (!theOneWithPath._2.isEmpty) {

            val parentOfTheOne = theOneWithPath._2.last

            parentOfTheOne.replace(theOneWithPath._1, rp)

            fol1

          } else
            rp

          fol2

        }

      }

      fol1 = newFOL

      if (fol2asp.debug)
        println("Updated formula in <removal of quantifiers>: " + fol1)

    }

    /** Convert to NNF (and also ConvConditional into Conditional) using the approach described in Cabalar et al. (see references),
      * which is itself an extension of a set of rules introduced in V. Lifschitz, L. R. Tang, H. Turner: Nested Expressions in Logic Programs */
    def convertToNNF(root: Sentence, depth: Int = 0): Sentence = {

      fol2asp.log(depthstr(depth) + "In NNF, checking " + root + "...")

      val newRoot: Sentence = root match {

        case Negation(Predicate("false", Nil)) => {
          genTrueFalseRules = true;
          Predicate("true", Nil)
        }

        case Negation(Predicate("true", Nil)) => {
          genTrueFalseRules = true;
          Predicate("false", Nil)
        }

        case Negation(Negation(Negation(s))) => convertToNNF(Negation(convertToNNF(s, depth + 1)))

        case Negation(Negation(s)) if !s.isInstanceOf[Predicate] => convertToNNF(Negation(convertToNNF(Negation(s), depth + 1)))

        case Negation(Conjunction(l)) => _root_.conversion.Disjunction(l.map(a => Negation(convertToNNF(a, depth + 1))))

        case Negation(Disjunction(l)) => _root_.conversion.Conjunction(l.map(a => Negation(convertToNNF(a, depth + 1))))

        case Negation(Conditional(p, c)) => _root_.conversion.Conjunction(List(convertToNNF(Negation(Negation(p)), depth + 1), convertToNNF(Negation(c), depth + 1)))

        case ConvConditional(c, p) => convertToNNF(Conditional(p, c), depth + 1)

        case Conjunction(l) => Conjunction(l.map(convertToNNF(_, depth + 1)))

        case Disjunction(l) => Disjunction(l.map(convertToNNF(_, depth + 1)))

        case Negation(s) if !s.isInstanceOf[Predicate] => Negation(convertToNNF(s, depth + 1))

        case Conditional(p, c) => Conditional(convertToNNF(p, depth + 1), convertToNNF(c, depth + 1))

        case x => x

      }

      newRoot

    }

    val fol2 = convertToNNF(fol1)

    if (fol2asp.debug)
      println("Updated formulas after NNF: " + fol2)

    // We apply rules (L1)-(L5) and (R1)-(R5) in Cabalar et al: "Reducing Propositional Theories in Equilibrium Logic to Logic Programs":

    val root = if (fol2.isInstanceOf[Conditional]) fol2 else {
      genTrueFalseRules = true;
      _root_.conversion.Conditional(_root_.conversion.Predicate("true", Nil), fol2)
    }

    def applyLRrules(root: Sentence /*must be in NNF, with NNF defined as in Cabalar et al (not the propositional NNF)*/ , depth: Int): Vector[Sentence] = {

      assert(root.isInstanceOf[Conditional])

      @inline def swapNested(sp: (Sentence, Sentence /*parent*/ ), isTarget: Sentence => Boolean, swapWith: (Sentence, Sentence /*parent*/ )): Boolean /*swap performed?*/ = {

        if (isTarget(sp._1)) {

          val tmpSubFormula = sp._1.deepClone

          sp._2.replace(sp._1, swapWith._1.deepClone)

          swapWith._2.replace(swapWith._1, tmpSubFormula)

          true

        } else {

          sp._1 match {

            case Negation(ss: Sentence) => swapNested((ss, sp._1), isTarget, swapWith)

            case Conditional(p: Sentence, c: Sentence) => {

              if (!swapNested((p, sp._1), isTarget, swapWith))
                swapNested((c, sp._1), isTarget, swapWith)
              else
                true

            }

            case Conjunction(p :: List(c)) => {

              if (!swapNested((p, sp._1), isTarget, swapWith))
                swapNested((c, sp._1), isTarget, swapWith)
              else
                true

            }

            case Disjunction(p :: List(c)) => {

              if (!swapNested((p, sp._1), isTarget, swapWith))
                swapNested((c, sp._1), isTarget, swapWith)
              else
                true

            }

            case _ => false

          }

        }
      }

      if (fol2asp.debug)
        println(depthstr(depth) + "Checking if in disjunctive LP form already: " + root) // not strictly necessary, but significantly simplifies results

      val faultsInAntecedents: (Vector[(Sentence, Vector[Sentence], Int)]) =
        root.asInstanceOf[Conditional].premise.traverseFilter(b = {
          case (s, _, _) => {

            s match {

              case s => !s.isInstanceOf[Predicate] &&
                ((!s.isInstanceOf[Negation] && !s.isInstanceOf[Conjunction])
                  ||
                  (s.isInstanceOf[Negation] && !s.asInstanceOf[Negation].sentence.isInstanceOf[Predicate]))

              //case _ => false

            }

          }
        }, path = Vector[Sentence](), noInAntecedents = 0, findMaxOne = true
        )

      lazy val faultsInConsequent: (Vector[(Sentence, Vector[Sentence], Int)]) =
        root.asInstanceOf[Conditional].conclusion.traverseFilter(b = {
          case (s, _, _) => {

            s match {

              case s => !s.isInstanceOf[Predicate] &&
                ((!s.isInstanceOf[Negation] && !s.isInstanceOf[Disjunction])
                  ||
                  (s.isInstanceOf[Negation] && !s.asInstanceOf[Negation].sentence.isInstanceOf[Predicate]))

              //case _ => false

            }

          }
        }, path = Vector[Sentence](), noInAntecedents = 0, findMaxOne = true
        )

      if (faultsInAntecedents.isEmpty && faultsInConsequent.isEmpty) {

        if (fol2asp.debug)
          println(depthstr(depth) + "Branch complete!")

        Vector(ConvConditional(root.asInstanceOf[Conditional].conclusion, root.asInstanceOf[Conditional].premise))

      } else {

        if (fol2asp.debug)
          println(depthstr(depth) + "No\n" + depthstr(depth) + "Now checking L1-L5, R1-R5, for " + root + "...")

        val newRoots: Vector[Sentence] = root match {
          // top-down, we don't recurse into sub-formulas here

          // L1 (we need this and L2 because true and false can be introduced by other transformations):

          case Conditional(Conjunction(Predicate("true", Nil) :: List(a)), b) => {

            fol2asp.log(depthstr(depth) + "L1a matched")

            Vector(_root_.conversion.Conditional(a, b))

          }

          case Conditional(Conjunction(a :: List(Predicate("true", Nil))), b) => {

            fol2asp.log(depthstr(depth) + "L1b matched")

            Vector(_root_.conversion.Conditional(a, b))

          }

          // L2:

          case Conditional(Conjunction(Predicate("false", Nil) :: List(a)), b) => {

            fol2asp.log(depthstr(depth) + "L2a matched")

            Vector[Sentence]()

          }

          case Conditional(Conjunction(a :: List(Predicate("false", Nil))), b) => {

            fol2asp.log(depthstr(depth) + "L2b matched")

            Vector[Sentence]()

          }

          // L3 (with commutativity of and/or, also in other rule applications):

          case Conditional(Conjunction(l@(Negation(Negation(p)) :: a :: Nil)), b) => {
            // (NB, also further cases: our conjunctions and disjunctions are 2-ary only)

            fol2asp.log(depthstr(depth) + "L3a matched")

            Vector(_root_.conversion.Conditional(a, _root_.conversion.Disjunction(_root_.conversion.Negation(p) :: List(b))))

          }

          case Conditional(Conjunction(a :: List(Negation(Negation(p)))), b) => {

            fol2asp.log(depthstr(depth) + "L3b matched")

            Vector(_root_.conversion.Conditional(a, _root_.conversion.Disjunction(_root_.conversion.Negation(p) :: List(b))))

          }

          // L4:

          case Conditional(Conjunction(Disjunction(p :: List(psi)) :: List(a)), b) => {

            fol2asp.log(depthstr(depth) + "L4a matched")

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(p :: List(a)), b),
              _root_.conversion.Conditional(_root_.conversion.Conjunction(psi :: List(a)), b))

          }

          case Conditional(Conjunction(a :: List(Disjunction(p :: List(psi)))), b) => {

            fol2asp.log(depthstr(depth) + "L4b matched")

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(p :: List(a)), b),
              _root_.conversion.Conditional(_root_.conversion.Conjunction(psi :: List(a)), b))

          }

          // (x | y) -> b (special case of L4 with a = true):

          case Conditional(Disjunction(x :: List(y)), b) => {

            fol2asp.log(depthstr(depth) + "L4c matched")

            Vector(_root_.conversion.Conditional(x, b),
              _root_.conversion.Conditional(y, b))

          }

          // L5:

          case Conditional(Conjunction(Conditional(p, psi) :: List(a)), b) => {

            fol2asp.log(depthstr(depth) + "L5a matched")

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(p) :: List(a)), b),
              _root_.conversion.Conditional(_root_.conversion.Conjunction(psi :: List(a)), b),
              _root_.conversion.Conditional(a, _root_.conversion.Disjunction(p :: List(Disjunction(_root_.conversion.Negation(psi) :: List(b))))))

          }

          case Conditional(Conjunction(a :: List(Conditional(p, psi))), b) => {

            fol2asp.log(depthstr(depth) + "L5b matched")

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(p) :: List(a)), b),
              _root_.conversion.Conditional(_root_.conversion.Conjunction(psi :: List(a)), b),
              _root_.conversion.Conditional(a, _root_.conversion.Disjunction(p :: List(Disjunction(_root_.conversion.Negation(psi) :: List(b))))))

          }

          // (p -> psi) -> b (special case of L5 where a = true):

          case Conditional(Conditional(p, psi), b) => {

            fol2asp.log(depthstr(depth) + "L5c matched")

            val a = _root_.conversion.Predicate("true", Nil)

            genTrueFalseRules = true

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(p) :: List(a)), b),
              _root_.conversion.Conditional(_root_.conversion.Conjunction(psi :: List(a)), b),
              _root_.conversion.Conditional(a, _root_.conversion.Disjunction(p :: List(_root_.conversion.Disjunction(_root_.conversion.Negation(psi) :: List(b))))))

          }

          // R1:

          case Conditional(a, Disjunction(Predicate("false", Nil) :: List(b))) => {

            fol2asp.log(depthstr(depth) + "R1a matched")

            Vector(_root_.conversion.Conditional(a, b))

          }

          case Conditional(a, Disjunction(b :: List(Predicate("false", Nil)))) => {

            fol2asp.log(depthstr(depth) + "R1b matched")

            Vector(_root_.conversion.Conditional(a, b))

          }

          // R2:

          case Conditional(a, Disjunction(Predicate("true", Nil) :: List(b))) => {

            fol2asp.log(depthstr(depth) + "R2a matched")

            Vector[Sentence]()

          }

          case Conditional(a, Disjunction(b :: List(Predicate("true", Nil)))) => {

            fol2asp.log(depthstr(depth) + "R2b matched")

            Vector[Sentence]()

          }

          // R3:

          case Conditional(a, Disjunction(Negation(Negation(p)) :: List(b))) => {

            fol2asp.log(depthstr(depth) + "R3a matched")

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(p) :: List(a)), b))

          }

          case Conditional(a, Disjunction(b :: List(Negation(Negation(p))))) => {

            fol2asp.log(depthstr(depth) + "R3b matched")

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(p) :: List(a)), b))

          }

          case Conditional(a, Negation(Negation(p))) => {

            fol2asp.log(depthstr(depth) + "R3c matched")

            genTrueFalseRules = true

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(p) :: List(a)), _root_.conversion.Predicate("false", Nil)))

          }

          // R4:

          case Conditional(a, Disjunction(Conjunction(p :: List(psi)) :: List(b))) => {

            fol2asp.log(depthstr(depth) + "R4a matched")

            Vector(_root_.conversion.Conditional(a, _root_.conversion.Disjunction(p :: List(b))),
              _root_.conversion.Conditional(a, _root_.conversion.Disjunction(psi :: List(b))))

          }

          case Conditional(a, Disjunction(b :: List(Conjunction(p :: List(psi))))) => {

            fol2asp.log(depthstr(depth) + "R4b matched")

            Vector(_root_.conversion.Conditional(a, _root_.conversion.Disjunction(p :: List(b))),
              _root_.conversion.Conditional(a, _root_.conversion.Disjunction(psi :: List(b))))

          }

          case Conditional(a, Conjunction(p :: List(psi))) => {

            fol2asp.log(depthstr(depth) + "R4c matched")

            Vector(_root_.conversion.Conditional(a, p),
              _root_.conversion.Conditional(a, psi))

          }

          // R5:

          case Conditional(a, Disjunction(Conditional(p, psi) :: List(b))) => {

            fol2asp.log(depthstr(depth) + "R5a matched")

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(p :: List(a)), _root_.conversion.Disjunction(psi :: List(b))),
              _root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(psi) :: List(a)), _root_.conversion.Disjunction(_root_.conversion.Negation(p) :: List(b))))
          }

          case Conditional(a, Disjunction(b :: List(Conditional(p, psi)))) => {

            fol2asp.log(depthstr(depth) + "R5b matched")

            Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(p :: List(a)), _root_.conversion.Disjunction(psi :: List(b))),
              _root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(psi) :: List(a)), _root_.conversion.Disjunction(_root_.conversion.Negation(p) :: List(b))))

          }

          case r5@Conditional(a, Conditional(p, psi)) => {

            fol2asp.log(depthstr(depth) + "R5c matched")

            //see [3], (R5'):

            if (!config.r5dash) {

              Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(p :: List(a)), psi),
                _root_.conversion.Conditional(_root_.conversion.Conjunction(_root_.conversion.Negation(psi) :: List(a)), _root_.conversion.Negation(p)))

            } else {  // deprecated

              // we use [3] R5' and also remove true ->

              assert(false) // TODO: check again; we would get strange results with this branch, ?seems one necessary "not" is ignored by R5'

              r5 match {

                case Conditional(Predicate("true", _), Conditional(p, psi)) => Vector(_root_.conversion.Conditional(p, psi))

                case _ => Vector(_root_.conversion.Conditional(_root_.conversion.Conjunction(p :: List(a)), psi))

              }
            }

          }

          case s => {
            // We need to move a deeply nested Disjunction or Conditional into the scope of above rule transformations,
            // using laws of associativity (+ commutativity, to make sure we catch commutativity along the way):

            lazy val copyR = root.deepClone

            val assoc1: Option[Vector[Sentence]] = s match {

              case Conditional(Conjunction(_), _) => {

                copyR match {

                  case Conditional(conj@Conjunction(a :: List(b)), c) => {

                    /* If a is a connective which isn't a conjunction, we just swap a and b.
                    Otherwise, we apply (in swapNested) the law of associativity, e.g., (x & y) & z becomes (z & y) & x.
                    This way, we bring the nested subformula x to the outer level.
                    */

                    if (swapNested((a, conj), (s => {
                      !s.isInstanceOf[Conjunction] &&
                        !s.isInstanceOf[Predicate] &&
                        (!s.isInstanceOf[Negation] || !s.asInstanceOf[Negation].sentence.isInstanceOf[Predicate])
                    }), (b, conj)))
                      Some(Vector(copyR))
                    else
                      None

                  }

                }

              }

              case _ => None

            }

            assoc1.getOrElse {

              val assoc2: Option[Vector[Sentence]] = s match {

                case Conditional(Conjunction(_), _) => {

                  copyR match {

                    case Conditional(conj@Conjunction(a :: List(b)), c) => {

                      if (swapNested((b, conj), (s => {
                        // that a and b are reverted is the only difference to assoc1
                        !s.isInstanceOf[Conjunction] &&
                          !s.isInstanceOf[Predicate] &&
                          (!s.isInstanceOf[Negation] || !s.asInstanceOf[Negation].sentence.isInstanceOf[Predicate])
                      }), (a, conj)))
                        Some(Vector(copyR))
                      else
                        None

                    }

                  }

                }

                case _ => None

              }

              assoc2.getOrElse {

                val assoc3: Option[Vector[Sentence]] = s match {

                  case Conditional(_, Disjunction(_)) => {

                    copyR match {

                      case Conditional(p, disj@Disjunction(a :: List(b))) => {

                        if (swapNested((a, disj), (s => {
                          !s.isInstanceOf[Disjunction] &&
                            !s.isInstanceOf[Predicate] &&
                            (!s.isInstanceOf[Negation] || !s.asInstanceOf[Negation].sentence.isInstanceOf[Predicate])
                        }), (b, disj)))
                          Some(Vector(copyR))
                        else
                          None

                      }

                    }

                  }

                  case _ => None

                }

                assoc3.getOrElse {

                  val assoc4: Option[Vector[Sentence]] = s match {

                    case Conditional(_, Disjunction(_)) => {

                      copyR match {

                        case Conditional(p, disj@Disjunction(a :: List(b))) => {

                          if (swapNested((b, disj), (s => {
                            // that a and b are reverted is the only difference to assoc3
                            !s.isInstanceOf[Disjunction] &&
                              !s.isInstanceOf[Predicate] &&
                              (!s.isInstanceOf[Negation] || !s.asInstanceOf[Negation].sentence.isInstanceOf[Predicate])
                          }), (a, disj)))
                            Some(Vector(copyR))
                          else
                            None

                        }

                      }

                    }

                    case _ => None

                  }

                  assoc4.getOrElse {

                    // None of the rules (L1-L4, R1-R5, associativity) matched. We convert the top-level conditional into a converse conditional
                    // (also indicates no further recursion required)

                    s match {

                      case Conditional(p, c) => Vector[Sentence](_root_.conversion.ConvConditional(c, p))

                    }

                  }

                }

              }

            }

          }

        }

        newRoots.flatMap {

          case r@Conditional(p, c) => applyLRrules(convertToNNF(r, depth + 1), depth + 1)

          case tlr => Vector(tlr)

        }

      }

    }

    val fol3 = applyLRrules(root, 0)

    fol2asp.log("Result of applyLRrules:\n  " + fol3.mkString("\n  "))

    /** Sentence s must be a disjunctive LP rule already, we (almost!) only change the symbols */
    def writeLP(s: Sentence): String = {

      s match {

        case ConvConditional(h, b) => {

          def collectLits(hb: Sentence): Vector[Sentence] = {

            hb match {

              case Conjunction(a :: List(b)) => collectLits(a) ++ collectLits(b)

              case Disjunction(a :: List(b)) => collectLits(a) ++ collectLits(b)

              case x => Vector(x)

            }

          }

          val headLitsR = collectLits(h)

          val bodyLits = collectLits(b)

          val (addBodyLits, headLits) = headLitsR.partition(_.isInstanceOf[Negation]) // all "not l" in the head become "{not l}0" or "not not l" (Gringo>=4) in the body. The latter is
          // better because it avoids nested aggregates (not allowed) in case l is an aggregate.

          val headLitsLP = headLits.map(_.forLP(config.notnotLiterallyAllowed)).filterNot(_ == "false")

          val r: String = headLitsLP.mkString(" " + config.disjSymbol + " ") + " :- " + (addBodyLits.map {

            //case Negation(p) => (if(config.notnotLiterallyAllowed) { p match { case Negation(pp) => pp.forLP(config.notnotLiterallyAllowed); case _ => "not not " + p.forLP(config.notnotLiterallyAllowed) } } else "{not " + p.forLP(config.notnotLiterallyAllowed) + "}0")

            l => Negation(l).forLP(config.notnotLiterallyAllowed)

          } ++ bodyLits.map(_.forLP(config.notnotLiterallyAllowed))).mkString(", ")

          val domainPreds = domainPredBindings.filter(db => r.contains(db._1)).map(_._2)

          if (domainPreds.isEmpty)
            r + "."
          else
            r + "," + domainPreds.mkString(",") + "."

        }

        case _ => {

          System.err.println("Error: fol2asp: assertion failed for " + s + " in writeLP")

          sys.exit(-1)

          ""
        }

      }

    }

    val lp = fol3.map(writeLP(_)).mkString("\n")

    fol2asp.log("Logic program:\n" + lp)

    Some(lp)
  }

  @inline def generateNewPredName(config: Fol2aspConfig) =
    config.prefixnp + newPredCounter.getAndAdd(1l) + (if (config.moduleIndex > 0l) "_" + config.moduleIndex else "")

  @inline def memoize[I, O](f: I => O): mutable.HashMap[I, O] = new scala.collection.mutable.HashMap[I, O]() {
    override def apply(x: I) = getOrElseUpdate(x, f(x))
  }

  val cache = new scala.collection.concurrent.TrieMap[(String, Fol2aspConfig), Option[String]]()

  /**
    * @author Matthias Nickles
    *
    * @param a
    * @return
    */
  def fol2aspSingleMemo(a: (Either[String, Sentence], Boolean, Fol2aspConfig)): Option[String] = {  // TODO: inactive for now (needs
    // perhaps more work wrt. variable naming if the same translation is copied into the output multiple times)

    if (a._1.isRight)
      fol2aspSingle(a._1, a._2, a._3)
    else {

      val key = (a._1.left.get, a._3) // too risky to leave out config, although normally the same configuration is used for a certain program run

      val c = cache.get(key)

      if (c.isDefined) {

        c.get

      } else {

        val r = fol2aspSingle(a._1, a._2, a._3)

        if (fol2asp.cacheTranslations)
          cache.update(key, r)

        r

      }

    }

  }

}
