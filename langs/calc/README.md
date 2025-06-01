# ```calc/```

## Overview
A parser and evaluator for arithmetic expressions given in string inputs; supporting addition, subtraction, multiplication, and division operations.


## Motivation
The calculator exists as a practical example demonstrating how Forja can be leveraged to parse languages and manipulate ASTs, with many of Forja's core features such as ```Wellformed```, ```PassSeq```, and ```SeqPattern``` being used.


## Components

### 1. ```package.scala```
Defines all of the ```Token``` types that are used and provides a wellformed definition representing how the AST should be structured once fully built.


### 2. ```CalcReader.scala```
```CalcReader``` is the lexer that converts input strings into tokens. 

It contains a wellformed definition of the initial token types with ```Number``` tokens that're wrapped around ```Expression``` and ```Op``` tokens for operations.
To support arithmetic grouping, `()` are matched as nested `Group` tokens, which is used by the parser to force precedences not implied by the operators (like most programming language parsers).

The ```rules``` method uses byte-level pattern matching to create tokens for numbers and operators, skipping whitespace and flagging unrecognized bytes as invalid.

### 3. ```CalcParser.scala```
```CalcParser``` is the parser, transforming the flat list of tokens into a structured AST.

The wellformed definition adds new ```Operation``` token types that have 2 ```Expression``` children. Also, ```Expression``` tokens have a new definition, being able to wrap both ```Number``` tokens as well as ```Operation``` tokens.

The passes are a sequence of transformations defined in the `passes` field, in this consisting of 2 binary operation parsing passes, and 2 passes related to `Group` tokens, which represent grouping with `()`.

First, the `GroupIsExpression` pass makes `Group` nodes into `Expression` nodes, so that arithmetic operator parsing treats them as already parsed objects.
Then, ```MulDivPass``` and ```AddSubPass``` both create nested expressions, replacing instances of `Expression Op Expression` with a single `Expression` node that looks like this:
```
Expression(
  Operation(
    Expression,
    Expression
  )
)
```

```MulDivPass``` is executed before ```AddSubPass``` to create precedence, allowing multiplication and division operations to be nested deeper than addition and subtraction operations in the AST.

Finally, the `StripGroups` pass removes nested `Expression(Group(Expression(...)))` objects, replacing them with the innermost `Expression`.
This is the only valid case, where the group could be parsed as a single nested expression.
In cases where there is a group but the nested expression could not be parsed (or there was no nested expression, as in the text `( )`), we could add extra rules to perform custom error reporting.
Interestingly, unlike in other parsing methods, this rule will have easy idiomatic access to both the incomplete parse, and the surrounding `()` context.
With the right rules, this can enable rich error reporting.

### 4. ```CalcEvaluator.scala```
```CalcEvaluator``` simplifies the AST and computes the value of the arithmetic expression.

The wellformed definition is imported from ```package.scala```, picking up with the AST structure of where ```CalcParser``` left off.
Each pass is annotated with its own input / output grammars, expressed as changes to the previous pass's output grammar.
- `ConstantsPass` converts each number to a native Scala `Int` for easier arithmetic, showing an example of the `Node.Embed` feature.
- `EvaluatorPass` is a ruleset that describes basic arithmetic evaluation, which will fold a wellformed tree into a single node of the form `Expression(Node.Embed[Int](???))`.
- `StripExpressionPass` simply cleans up the previous pass's tree, leaving just `Node.Embed[Int](???)` containing the result of evaluating the expression.


## Usage
To learn how to use the calculator, ```package.test.scala``` contains methods (```parse```, ```read```, ```evaluate```) that execute the different components of the calculator.


## Example
The following images demonstrates the state of the AST after each pass with the input "5 + 3 * 4". Viewing the state of the AST after each pass can also be done by running calculator test cases with the tracer enabled. 

![example1](img/example1.jpg)
![exampel2](img/example2.jpg)
