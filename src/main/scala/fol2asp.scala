/*
 * fol2asp
 *
 * Copyright 2016-2019 by Matthias Nickles
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
 *
 */

/*

  To do:
   - More detailed API documentation
   - Better error messages
   - A few minor inline TODOs (see source files)

 */

package commandline

import conversion.Converter
import misc.ASPIOutils

/**
  * @author Matthias Nickles
  */
object fol2asp {

  val debug = false

  val version = "0.4"

  val versionAndLicenseText = "fol2asp " + version + "\nCopyright (c) 2016-2019 Matthias Nickles\nLicense: https://www.apache.org/licenses/LICENSE-2.0\n"

  val defaultNewVarPrefix = "_NVAR_"  // can be overridden using --prefixnv

  val defaultNewPredPrefix = "_npred_"  // can be overridden using --prefixnp

  val strLitPrefix = "__strlit_"

  val cacheTranslations = false  // leave false for now

  val helpText =
    versionAndLicenseText +
      """
  fol2asp translates an enconding consisting of First Order Logic (FOL)
  formulas (under stable model semantics) into an Answer Set program (a
  logic program). The input can also comprise answer set rules or facts -
  these are copied unmodified to the output. fol2asp also has preliminary
  support for the translation of Markov Logic Network (MLN)-style syntax
  to ASP syntax.

  Command line usage:

  java -jar fol2asp.jar [<file1> ... <fileN>] [--mlnrules] [--prefixnp p]
   [--prefixnv p] [--gringo3] [--retainfol] [--omitcomments]
   [--version|-v] [--help|-h]

  <file1> ... reads input from file instead of stdin
  --version|-v prints version and license information, then exits
  --help|-h prints this text and exits
  --prefixnp p prepends p to newly introduced predicate names. Max. length 32.
  --prefixnv p prepends p to newly introduced variable names. Max. length 32.
  --mlnrules enables support for MLN-style clause and rule syntax (see below)
  --strongexcl changes the meaning of operator ! from default negation to
    strong negation
  --retainfol copies the original FOL formulas to the ouput as comments in
    front of their translations
  --omitcomments omits user-specified comments in the output
  --gringo3 increases compatibility of the output with Lparse and older
    versions of Potassco gringo/clingo. Also, without --gringo3, any
    #domain declarations will be commented in the output (even though
    fol2asp can still use them to obtain information about variable bindings).

  fol2asp recognizes the following FOL connectives and quantifiers (ordered by
  precedence, lowest first):

  FORALL X,Y,Z,dx(X),dy(Y),dz(Z):f where predicates dx/1, dy/1 and dz/1
  specify the domains (ranges) of variables X, Y and Z
  EXIST X,Y,Z,dx(X),dy(Y),dz(Z):f (analogously to FORALL)
  f <-> g   biconditional
  f <- g    converse implication
  f -> g    implication
  f | g     disjunction
  f & g     conjunction
  not f     default negation
  !f        negation (default negation by default, or strong negation with
            --strongexcl)
  -f        strong (classical) negation
  true      truth
  false     falsity

  Parentheses can be used to change precedences or to make precedence explicit.
  All formulas must end with a dot (e.g., "FORALL A, number(A): even(A).")
  Only integer and "string" literals are allowed in term positions.
  Comments have the form %... or //... (single line) and %*...*% or /*...*/
  (multiline).
  Any Clingo script (e.g., Python or Lua) needs to be placed at the beginning
  of the input. Only one script is allowed. The script is simply copied to
  the output (unchecked).

  ASP and FOL syntax cannot be mixed within the same formula or rule.

  Quantifier syntax "FORALL X,Y,Z:" and "EXIST X,Y,Z:" where the variables are
  bound to ranges somewhere else (e.g., using certain atoms, or #domain
  declarations for older gringo versions) is supported too.

  With --mlnrules, formulas can also have the following syntax:
    p1, p2, ... => q1 v q2 v ...
    (MLN-style rule. "," or "^" denotes conjunction, " v " is disjunction
    (observe the required spaces around "v"), the p_i and q_i are atoms.
    Variables are considered universally quantified ASP variables.)
    EXIST X,Y,... p1, p2, ... => q1 v q2 v ...
    (as above but with existentially quantified variables X,Y,...)
    l1 v l2 v l3 v ... (MLN-style clause. Use !p for negative literals; note
    that the meaning of !p is, by default, default negation, can be switched
    to classical negation using --strongexcl.
    Observe the required spaces around "v".
    EXIST X,Y,... l1 v l2 v l3 v ... (as above but with existentially
    quantified variables X,Y,...)
    Note (1) that even in MLN-style formulas the first letter in a variable
      name must be uppercase and the first letter in a term must be lower
      case letters, as required by most ASP grounders. The first letter in
      predicates should be lower case.
    Note (2) that using MLN-style syntax for rules and clauses does not give
      these formulas MLN semantics; they just use different symbols for
      connectives compared to our default syntax. Still, they might make the
      translation of MLN encodings easier.
    Note (3) that MLN-style formulas require that variable domains are specified
      (as usual in Markov Logic Network programs). You can do this in fol2asp
      using #domain declarations. fol2asp will generate the correct "inline"
      bindings (using atoms in rule bodies) from these. If flag --gringo3
      is not specified, the #domain declarations do not appear in the resulting
      logic program (useful with Gringo/Clingo >3).
      Example:
        #domain d(X;Y).  // "predicate schema"
        #domain r(Z).
        EXIST X,Y,Z p(X) v !p(Y) v q(Z)  // MLN-style clause

  Symbol prefixes __aux_, """ + strLitPrefix + """, """ + defaultNewPredPrefix + """ and """ + defaultNewVarPrefix + """
  are reserved by default and should not be used in any symbols in the input.
  However, it is possible to define different prefixes using console arguments
  --prefixnp and --prefixnv (see above).

  Observe that the syntax supported by fol2asp is similar but not identical to
  that used by tool F2LP. E.g., precedences of connectives are different
  (fol2asp uses precedences closer to those usually used with Boolean
  expressions in programming languages), and in fol2asp the scope of
  quantifiers always extends to the right as far as possible
  (e.g., "FORALL X: a(X) -> b(X)" means "FORALL X: (a(X) -> b(X))"). There
  is no special position for "<-" (it can be used anywhere in a formula where
  a connective is allowed).
  Aggregates in FOL formulas are not parsed and treated as black-box atoms (and
  should be completely avoided within FOL formulas).
  "-" is always treated as strong negation. - and ! can extend over
  arbitrary subformulas (using parentheses, e.g., -(a & not b)).

  Observe that any parts of the input which are not recognized are copied to
  the output unchecked and unmodified (including any unrecognized parts of
  formulas and terms).
  """

  object MessageTypes extends Enumeration {

    type MessageType = Value

    val INFO, WARNING, ERROR = Value

  }

  import MessageTypes._

  val stompMessages = Map(

    -1 -> ("Invalid command line argument(s)", ERROR),

    -2 -> ("File error", ERROR),

    -3 -> ("External program call failed", ERROR),

    -4 -> ("I/O error", ERROR),

    -200 -> ("Invalid format of input data from STDIN", ERROR),

    -201 -> ("Invalid prefix specified with --prefixnp ", ERROR),

    -202 -> ("Invalid prefix specified with --prefixnv ", ERROR),

    -300 -> ("Syntax error", ERROR), // Note that we don't perform a full syntax check of the input

    -301 -> ("Internal error", ERROR),

    -302 -> ("Possibly unbound quantifier variable", WARNING),

    -303 -> ("Found multiple bindings for quantifier variable", WARNING)

  )

  def stomp(code: Int, additionalInfo: String = "") = {

    val message = stompMessages(code)

    val messageBody = message._1 + " " + additionalInfo

    if (message._2 == ERROR) {

      System.err.println("\nERROR: " + messageBody)

      sys.exit(code)

    } else if (message._2 == WARNING)
      System.out.println("% WARNING: " + messageBody)
    else
      System.out.println("% INFO: " + messageBody)

  }

  @inline def log(debugMessage: => Any): Unit = { // TODO: use Java logger

    if (debug) {

      System.out.println(debugMessage.toString)

      System.out.flush()

    }

  }

  def main(args: Array[String]): Unit = {

    var omitSysExit0 = false // If .jar is dynamically included in some calling program using classloader, we must not sys.exit(0) in case of successful
    // termination (except -v/-h), as this would quit the overall program. We could prevent this issue using some additional wrapper method, but we want to
    // keep the caller compatible with Java tools other than this.
    // If on the other hand the tool is invoked as an external process on operating system level, sys.exit(0) is required.

    val arglist = args.toList

    type ArgsList = List[(Symbol, List[String])]

    def nextArg(argsList: ArgsList, list: List[String]): ArgsList = {

      @inline def isSwitch(s: String) = (s(0) == '-')

      list match {

        case Nil => argsList

        case ("--version" | "-v") :: tail => {

          println(versionAndLicenseText)

          sys.exit(0)

        }

        case ("--help" | "-h") :: tail => {

          println(versionAndLicenseText + "\n\n" + helpText)

          sys.exit(0)

        }

        case ("--omitSysExit0") :: tail => { // used internally when called via dynamic jar loading (see above)

          omitSysExit0 = true

          nextArg(argsList, tail)

        }

        case "--gringo3" :: tail => {

          nextArg(argsList ++ List('forGringo3 -> List("")), tail)

        }

        case "--mlnrules" :: tail => {

          nextArg(argsList ++ List('mlnrules -> List("")), tail)

        }

        case "--retainfol" :: tail => {

          nextArg(argsList ++ List('retainfol -> List("")), tail)

        }

        case "--strongexcl" :: tail => {

          nextArg(argsList ++ List('strongexcl -> List("")), tail)

        }

        case "--omitcomments" :: tail => {

          nextArg(argsList ++ List('omitcomments -> List("")), tail)

        }

        case "--prefixnp" :: p :: tail => {

          if(p.isEmpty || !p.forall((c: Char) => c == '_' || c.isLetterOrDigit) || p.length > 32 ||
            !(p.head.isLower || p.head == '_' && p.length >= 2 && p(1).isLower))
            stomp(-201)

          nextArg(argsList ++ List('prefixnp -> List(p)), tail)

        }

        case "--prefixnv" :: p :: tail => {

          if(p.isEmpty || !p.forall((c: Char) => c == '_' || c.isLetterOrDigit) || p.length > 32 ||
            !(p.head.isUpper || p.head == '_' && p.length >= 2 && p(1).isUpper))
            stomp(-202)

          nextArg(argsList ++ List('prefixnv -> List(p)), tail)

        }

        case fileName :: tail if !isSwitch(fileName) => {

          nextArg(argsList ++ List('convertFromFile -> List(fileName)), tail)

        }

        case option :: tail => stomp(-1, option); argsList

      }

    }

    val parsedArgs: ArgsList = nextArg(Nil, arglist)

    log("Arguments in fol2asp: " + parsedArgs)

    val forGringo3 = parsedArgs.exists(_._1 == 'forGringo3)

    val retainfol = parsedArgs.exists(_._1 == 'retainfol)

    val strongexcl = parsedArgs.exists(_._1 == 'strongexcl)

    val mlnrules = parsedArgs.exists(_._1 == 'mlnrules)

    val omitcomments = parsedArgs.exists(_._1 == 'omitcomments)

    val prefixnp: String = parsedArgs.filter(_._1 == 'prefixnp).flatMap(_._2).headOption.getOrElse(defaultNewPredPrefix)

    val prefixnv: String = parsedArgs.filter(_._1 == 'prefixnv).flatMap(_._2).headOption.getOrElse(defaultNewVarPrefix)

    val folaspProgs: List[String] = if (parsedArgs.exists(_._1 == 'convertFromFile)) parsedArgs.filter(_._1 == 'convertFromFile).map(
      sv => ASPIOutils.slurpFromFile(sv._2.head))
    else
      List(ASPIOutils.readAllLinesFromStdIn.mkString("\n"))

    folaspProgs.foreach(folaspProg => {

      log("fol2asp input program:\n" + folaspProg + "\n-----")

      val conversionResult: String = Converter.fol2aspProg(folaspProg, verbose = true, refeedD7b = true,
        moduleIndex = 0, notnotLiterallyAllowed = !forGringo3, disjSymbol = (if (!forGringo3) ";" else "|"),
        prefixnp = prefixnp, prefixnv = prefixnv, retainfol = retainfol, mlnrules = mlnrules, omitcomments = omitcomments,
        strongexcl = strongexcl, omitDomainDecls = !forGringo3)

      System.out.println(conversionResult)

      if (false) { // for debugging purposes only (to check if resulting ASP program can be brought into normal form. An issue might be "real" disjunction vs. some choice rule)

        val clingoStdout = ASPIOutils.externalCmdWithInput("clingo --trans-ext=all --pre=aspif", Some(Left(conversionResult)), None).get._1.toList

        System.out.println("\n\nClingo --trans-ext=all --pre=aspif:\n" + clingoStdout.mkString("\n") + "\n-------------")

      }

    })

    if (!omitSysExit0)
      sys.exit(0)

  }

}
