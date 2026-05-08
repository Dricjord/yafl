# Yafl – Yet Another Functional Language

A toy implementation of a tiny programming language.

## Syntax

The syntax of Yafl is defined below.
Identifers are strings of alphanumeric characters and the underscore, starting with a non-numeric character (e.g., foo or _23).

```
term ::=
  | unit-literal
  | boolean-literal
  | integer-literal
  | identifier
  | infix-application
  | term-abstraction
  | term-application
  | type-abstraction
  | type-application
  | '(' term ')'

unit-literal ::=
  | '(' ')'

infix-application ::=
  | term operator term

term-abstraction ::=
  | '(' identifier ':' type (',' identifier ':' type)* ')' '=>' term

term-application ::=
  | term term

type-abstraction ::=
  | '[' identifier (',' identifier)* ']' '=>' term

type-application ::=
  | term '[' type (',' type)* ']'

type ::=
  | identifier
  | arrow
  | forall
  | '_'

type arrow ::=
  | type -> type

type forall ::=
  | '[' identifier (',' identifier)* ']' => type
```
