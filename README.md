# lambda
A lazily evaluatated implementation of the lambda calculus with support for user definitions of inline terms.

##Grammar
```
<lambda> -> <definition>*
<definition> -> var ":=" <lambdaTerm>
<lambdaTerm> -> var | <abstraction> | <application>
<abstraction> -> '\' <var> '.' <lambdaTerm>
<application> -> '(' <lambdaTerm> <lambdaTerm> ')'
```
