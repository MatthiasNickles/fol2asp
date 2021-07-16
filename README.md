### fol2asp ###

- [Introduction](#introduction)
- [Installation and call](#installation-and-call)
- [Usage](#usage)
- [Author contact details](#author--contact-details)
- [fol2asp copyright & license](#delsat-copyright--license)

#### Introduction ####

fol2asp translates encondings consisting of formulas in First-Order Logic (FOL) syntax (under stable model semantics) 
into Answer Set Programs (Logic Programs). The input can also comprise answer set rules - these are 
copied unmodified to the output. Besides supporting the usual FOL syntax, fol2asp can also translate 
Markov Logic Network (MLN)-style _hard_ (non-probabilistic) rules into ASP syntax. 

The resulting answer set program can be grounded and solved 
using, e.g., [Clingo](https://potassco.org/clingo/) or [Lparse/Smodels](http://www.tcs.hut.fi/Software/smodels/) (depending
to the ASP features used).

fol2asp is inspired by [F2LP](http://reasoning.eas.asu.edu/f2lp/index.html) and uses almost the same logic conversion rules (see references below). However,
besides running on the Java Virtual Machine (JVM), fol2asp provides better compatibility with recent versions 
of Gringo/Clingo compared to F2LP 1.3 and several additional features (see under Sect. [Usage](#usage)). Input syntax is similar but not identical (see next section).
  
References:
- [1] Joohyung Lee, Ravi Palla (‎2012): "Reformulating the Situation Calculus and the Event Calculus in the General
       Theory of Stable Models and in Answer Set Programming". In Journal of Artificial Intelligence Research 43, 571-620.
- [2] Joohyung Lee, Ravi Palla (2009) "System f2lp – Computing Answer Sets of First-Order Formulas". In: Erdem E., Lin F., Schaub T. (Eds) Logic Programming 
       and Nonmonotonic Reasoning (LPNMR 2009). Lecture Notes in Computer Science (LNCS), vol 5753. Springer.
- [3] Pedro Cabalar, David Pearce, Agustin Valverde (‎2005): "Reducing Propositional Theories in Equilibrium Logic to Logic Programs". 
       In Bento C., Cardoso A., Dias G. (eds) Progress in Artificial Intelligence. EPIA 2005. Lecture Notes in Computer Science, vol 3808. Springer.  
       (for NNF, rules L1-L5, R1-R5).        
       [3] references [4] for some of the transformation rules:  
- [4] Vladimir Lifschitz, Lappoon R. Tang, Hudson Turner (1999): "Nested Expressions in Logic Programs". In Annals of Mathematics and Artificial Intelligence (1999) 25: 369.

#### Installation and call ###

fol2asp is written in Scala and runs on the JVM (JRE/JDK 8 or higher). A ready-to-run JAR file can be found under [Releases](https://github.com/MatthiasNickles/fol2asp/releases). 

Executable binaries for Linux, MacOS or Windows can probably be generated using GraalVM, but I haven't tried that yet.

To build from sources, use, e.g., [sbt](https://www.scala-sbt.org/) and sbt plugin assembly (just install sbt and enter "sbt assembly" on the commandline), 
or, e.g., Maven (since there are no dependencies besides the Scala standard library, this should be straightforward). 

Run fol2asp like this. The result is written to file example1.lp:

    java -jar fol2asp.jar examples/example1.fol > example1.lp
    
The list of available commandline parameters is shown with `--help` and in more detail in the following section.    
    
#### Usage ####

Command line usage:

    java -jar fol2asp.jar [<file1> ... <fileN>] [--mlnrules] [--prefixnp p] [--prefixnv p] [--gringo3] [--retainfol] [--omitcomments] [--version|-v] [--help|-h]

If no input file(s) `<file1> ...` are specified, input is read from STDIN 

`--version|-v` prints version and license information, then exits

`--help|-h` prints a help text and exits

`--prefixnp p` prepends p to newly introduced predicate names

`--prefixnv p` prepends p to newly introduced variable names 

`--mlnrules` enables support for MLN-style unweighted clause and rule syntax (see below)

`--strongexcl` changes the meaning of operator `!` from default negation to
strong negation

`--retainfol` copies the original FOL formulas to the ouput as comments in
front of their translations

`--omitcomments` omits user-specified comments in the output

`--gringo3` increases compatibility of the output with Lparse and older
versions of Potassco gringo/clingo. Also, without `--gringo3`, any
`#domain` declarations will be commented in the output (even though
fol2asp can still use them to obtain information about variable bindings).

fol2asp recognizes the following FOL connectives and quantifiers (ordered by
precedence, lowest first):

`FORALL X,Y,Z,dx(X),dy(Y),dz(Z):f` represents universal quantification, where predicates `dx/1`, `dy/1` and `dz/1`
specify the domains (ranges) of variables `X`, `Y` and `Z`, and `f` is a subformula.

`EXIST X,Y,Z,dx(X),dy(Y),dz(Z):f` (existential quantification, arguments analogously to FORALL)

`f <-> g`   biconditional

`f <- g`    converse implication

`f -> g`    implication

`f | g`     disjunction

`f & g`     conjunction

`not f`     default negation

`!f`        negation (default negation by default, or strong negation with switch
          `--strongexcl`)

`-f`        strong (classical) negation

Quantifier syntax `FORALL X,Y,Z:` and `EXIST X,Y,Z:` where the variables are
bound to ranges somewhere else (e.g., using certain atoms, or `#domain`
declarations for older gringo versions) is supported too.

Variable domains need to be finite.

Parentheses can be used to change precedences or to make precedence explicit.
All formulas must end with a period mark (full stop), e.g., 
`p(a,9) & FORALL A, number(A): not even(A).`

Integers and `"strings"` are allowed in term positions. Terms have a syntax 
as expected by common ASP grounders, e.g., `foo(fun(a,X),b)` is a valid (non-ground) 
term if the target grounder is gringo/clingo. However, (tuples) are currently 
not supported in FOL formulas (but in ASP rules).

Comments have the form `%...` or `//...` (single line comment) and `%*...*%` or `/*...*/` (multiline).

Any Clingo script (e.g., Python or Lua) needs to be placed at the beginning
of the input. Only one script is allowed. The script is simply copied to
the output (unchecked).

ASP and FOL syntax should not be mixed within the same formula or rule (though 
fol2asp does not check that and keeps unrecognized formula parts unmodified 
in the output).

If switch `--mlnrules` is provided, formulas can also have the following syntax
(in addition to FOL formulas using the syntax above and ASP rules). fol2asp
does not require such formulas to end with a period (.) but recall that 
in MLN, the period is used to indicate infinite weight (hard rule).

`p1, p2, ... => q1 v q2 v ...`

(MLN-style rule without weight. `,` or `^` denotes conjunction, ` v ` is disjunction
(observe that spaces are required around `v`), the `pi` and `qi` are atoms.
Any free variables are here considered to be universally quantified ASP variables.)

`EXIST X,Y,... p1, p2, ... => q1 v q2 v ...`

(as above but with existentially quantified variables `X`,`Y`,...)

`l1 v l2 v l3 v ...`

(MLN-style clause without weight. The `li` are literals. Use `!p` for negative 
literals; note that the meaning of `!p` is, by default, default negation, can 
be switched to classical negation using `--strongexcl`.
Observe the required spaces around `v`.

`EXIST X,Y,... l1 v l2 v l3 v ...` 
(as above but with existentially quantified variables `X`,`Y`,...)

Note (1) that even in MLN-style formulas the first letter in a variable
  name must be uppercase and the first letter in a term must be lower
  case letters, as required by most ASP grounders. The first letter in
  predicates should be lower case.

Note (2) that using MLN-style syntax for hard rules and clauses does not give
  these formulas MLN semantics; they just use different symbols for
  connectives compared to our default syntax. Still, they might make the
  translation of MLN encodings easier.

Note (3) that MLN-style formulas require that variable domains are specified
  (as usual in Markov Logic Network programs). You can do this in fol2asp
  using `#domain` declarations. fol2asp will generate the correct "inline"
  bindings (using atoms in rule bodies) from these. If flag `--gringo3`
  is not specified, the `#domain` declarations do not appear in the resulting
  logic program (useful with current Clingo/Gringo versions as these do not
  understand `#domain` declarations).
  
  Example for MLN-style input:
      
    d(10;20;30;40).  // domain d
    #domain d(X;Y).  // "predicate schema". X and Y are variables.    
    EXIST X,Y p(X) v !p(Y) v q(Y,X).  // MLN-style hard clause

Symbol prefixes `__aux_`, `__strlit_`, `_npred_` (fresh predicate) and 
`_NVAR_` (fresh variable) are reserved by default and should not be used 
in any symbols in the input. However, it is possible to define different 
prefixes for fresh predicates and variables using console arguments 
`--prefixnp` and `--prefixnv`. 
fol2asp might use and emit definitions for predicates `true/0` and 
`false/0` (truth and falsity).

Observe that the syntax supported by fol2asp is similar but not identical to
that used by software F2LP. E.g., precedences of connectives are different
(fol2asp uses precedences closer to those usually used with Boolean
expressions in programming languages), and in fol2asp the scope of
quantifiers always extends to the right as far as possible
(e.g., `FORALL X: a(X) -> b(X)` means `FORALL X: (a(X) -> b(X))`). There
is no special position for `<-` (it can be used anywhere in a formula where
a connective is allowed).
Aggregates in FOL formulas are not parsed and treated as black-box atoms (and
should be completely avoided within FOL formulas).
`-` is always treated as strong negation. `-` and `!` can extend over
arbitrary subformulas (using parentheses, e.g., `-(a & not b)`).

Observe that any parts of the input which are not recognized are copied to
the output unchecked and unmodified (including any unrecognized parts of
formulas and terms).    

#### Author contact details ####

Author: Matthias Nickles 

matthiasDOTnicklesATgmxDOTnet

Web: https://www.researchgate.net/profile/Matthias_Nickles

Feedback and bug reports are welcome.

#### fol2asp copyright & license ####

Copyright (c) 2016-2019 by Matthias Nickles

License: [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0)
