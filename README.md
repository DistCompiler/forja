# Developer Guide

Follow these steps to setup and test the project:

1. **Clone the Repository**  
   Clone this repository to your local machine:

   ```bash
   git clone <https://github.com/DistCompiler/forja.git>
   ```

2. **Install Scala CLI:**
   Install Scala CLI using the using this [link](https://scala-cli.virtuslab.org/install/)
3. **Install Plugin:**
   Install the Scala(Metals) plugin by Scalameta in VSCode, or equivalent for your IDE.

4. **Verify Installation:**
   After installation, close and reopen your terminal, then verify Scala CLI is installed by running:

   ```bash
   scala-cli version
   ```

5. **Run Tests:**
   After veryifying installation, build and run the tests using:

   ```bash
   scala-cli test .
   ```

6. **Source formatting:**
   When contributing, make sure you've formatted your code.
   This is checked by CI.

   I recommend committing your work, then running the formatters to see the formatting diff.
   You can then commit with `--amend` to replace your commit if you're happy with what the formatters did.

   Run all the formatting scripts with:
   ```bash
   scala-cli run . --main-class scripts.rewrite_src_sc
   ```

   As a convenience, you can also use `./rewrite_src.sh`, which is a one line shell script that launches the above command.

7. **Debugging:**
   You can try to use the debug adapter for troubleshooting and debugging. Refer to the documentation for setup and usage instructions.

# Forja Compiler Toolkit

Forja is a tool for building compilers, or other language transformation tools.

This repository is public for collaboration purposes, and makes no promises of completion.
This message will be replaced with links to documentation and such as the prototype gets closer to complete.

Top-level repo guide:
- `src`: the main Forja source code and tests.
- `langs`: languages implementations using Forja.
  Includes the `calc` demo language.
- `scripts`: utilities for maintaining the codebase.

Here are some key types defined in `src`:
- `forja.Node`: a generic AST class inspired by the Trieste project. AST nodes are tagged with Token objects, whose pointer identity makes for easy pattern matching. The structure is mutable, and is designed to have well defined behavior despite this fact (should be hard to create corrupt trees).
- `forja.manip.Manip`: an _applicative functor_ that is used for tree traversal and modification. Manip supports context-passing, backtracking search with commit points, and flatMap for occasions when monadic behavior is needed.
- `forja.manip.SeqPattern`: a wrapper around Manip that handles pattern matching sequences of sibling nodes in an AST. Used with `dsl.on(...)` and `dsl.pass`, patterns let you express rewrite rulesets.
- `forja.source.Source` and `forja.source.SourceRange`: byte-level views over files, wrapping `java.nio` buffers. Contains added Scala sequence syntax, no-copy slicing, line/column number calculation, and source location pretty-printing.
- `forja.wf.Wellformed`: AST specification and validation. You write what your AST should look like in an EBNF-like syntax and it:
  1. will check that syntax for you if you ask, wrapping bad cases in Error nodes. Node has facilities for efficiently detecting Error subtrees and iterating over them.
  2. can serialize your AST to and from s-expressions, either to look at it, or for interchange.

Outside of those notable classes and some utilities, there are 2 important subfolders:

- `forja.sexpr`: facilities for reading and writing S-Expressions. Mostly for use by Wellformed, but tested on a necessary-seeming subset of the original proposed RFC.
- `forja.langs.calc`: demo language and work in progress tutorial for getting to grips with Forja's concepts.
- `forja.langs.tla`: under-construction parser for the TLA+ modeling language, which has been invaluable in stress-testing all the generic machinery. The test suite involves trying to parse the full TLA+ Examples folder.
