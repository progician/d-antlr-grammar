# Overview

d-antlr-grammar is a software project to implement a parser and AST builder for the [D Programming Language](http://d-lang.com). It is an [ANTLR v3](http://www.antlr.org) grammar based project which generates a raw CommonTree object hierarchy which would be easy to convert into the [DDT]( https://code.google.com/a/eclipselabs.org/p/ddt/) project's AST hierarchy. At the later stages of this project the code base will be imported in to the [progician/ddt](https://github.com/progician/ddt) project and used as the primary interpreter.

The DDT project is currently using the descent.compiler package to implement the parser functionality and converts the descent.compiler's AST hierarchy in to it's own system. The descent.compiler project was abandoned a while ago, the only maintainer today is the maintainer of the DDT projectm, Bruno Medeiros. The source code of the descent.compiler project seems to be a C++ to Java transcript of an earlier version of the DMD source base. Walter, the maintainer of the DMD project has a hands-on approach to parsers and therefore the DMD project works with hand-made parser instead of parser generator. For a better maintainability however I propose to use an generated parser that is in line with the D language reference, but described in a EBNF-style notion like ANTLR. ANTLR was the obvious choice of tool but it is not unlikely that it will be switched to a more optimized platform on a later stage.

The project also represents the author's efforts to understand parsers, learn parser generators and to provide an up-to-date and maintainable parser for the DDT project. The base line of specification is to implement a D parser, completely compliant to the latest DMD language specification, that also fast enough to use in an Eclipse plugin, and also capable of producing human-readable error messages.

As a side-goal the project should be useful as a ground-work for a D expression evaluator for the GDB project.

# Install

The d-antlr-grammar project is essentially a simple Java test application for which you need to obtain the sources and build. As a requisite you'lln need the ANTLR parser generator. The currently working version is the v3.4 which is included in the package and therefore there's no need for extra installation.

You can check out the source code with the following command:

```bash
git clone git://github.com/progician/d-antlr-grammar.git
```

Note that this address is only refer to a read-only repository. The project depends on the antlr runtime which can be found in the same jar file as the parser generator. Check the d-parser/Makefile JDK_ROOT variable if points to the right JDK instance. Build the project:

```bash
make
make
```

You need to call twice the make when you start from scratch. The build will inevitably fail with some compiler errors in the first pass. For the moment I couldn't find any explanation for this behaviour. You can fix it if you like ;)

# Run

All you need to do is:

```bash
cd d-parser
java -jar dparser.jar
```

The program will automatically run through the test-sources directory and test each line against the AST structure described in the comment line before.

# Eclipse

The source tree also contains the Eclipse project file so it is possible to build and run the parser tests. As the ANTLR 3.4 package is included in the repository there's no need for any changes, apart from a properly set JRE+JDK.

# The AST Description Language

For testing purposes a simple AST Description Language was developed. It provides an easy way to test the output of the parser. Basically, the language is used as a simplistic tree description somewhat very similar to LISP (LIst Processing). Tree's are basically lists embedded in to lists. The AST node's type's name constitutes a leaf, a single node. If there are no children to a node, it is described only by the type name. A simple return statement with no arguments would look like this:

```
STMT_RETURN
```

You can also test the actual text of the token against your expectations:

```
STMT_RETURN["return"]
```

In this case if the node's type is STMT_RETURN, then the program goes on and test the text against the string "return". This comes handy when you testing tokens like Identity, or literals.
If a AST node has children it must be enclosed in to brackets:

```
// (STMT_RETURN EXP_INTEGER_LITERAL["0"])
return 2;
```

That's about it! There are three interesting information about a parsed source code: the node types themselves, the relation or hierachy between the nodes, and their respective values. The program goes through the source files in the test-sources directory, parse them up, and goes through all the actual source lines (skipping through the comment lines), and check the previous comment line for the AST expectation line, just like in the source snippet above.