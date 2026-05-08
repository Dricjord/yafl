# Yafl – Yet Another Functional Language

A toy implementation of a tiny functional programming language.

## Summary

Yafl is essentially an implementation of [System F](https://en.wikipedia.org/wiki/System_F) with a few extensions.
The following illustrates a simple program:

```yafl
((x: Int) => x + 1) 2
```

## Syntax

The syntax of Yafl is described by the production rules below.
The following is assumed:

- Integer literals are contiguous sequences of digits (e.g., `123`); and
- Identifers are strings of alphanumeric characters and the underscore, starting with a non-numeric character (e.g., `foo` or `_23`).

```
term ::=
  | term type-application
  | mul-term

mul-term ::=
  | mul-term ['*' | '/'] add-term
  | add-term

add-term ::=
  | add-term ['+' | '-'] comparison-term
  | comparison-term

comparison-term ::=
  | comparison-term ['==' | '!=' | '<' | '<=' | '>=' | '>']  and-term
  | and-term

and-term ::=
  | and-term '&&' or-term
  | or-term

or-term ::=
  | or-term '&&' prefix-term
  | prefix-term

prefix-term ::=
  | ['!' | '-'] prefix-term
  | type-application

type-application ::=
  | type-application '[' type (',' type)* ']'
  | simple-term

simple-term ::=
  | unit-literal
  | boolean-literal
  | integer-literal
  | identifier
  | term-abstraction
  | type-abstraction
  | conditional
  | '(' term ')'

unit-literal ::=
  | '(' ')'

boolean-literal ::=
  | 'true'
  | 'false'

term-abstraction ::=
  | '(' identifier ':' type (',' identifier ':' type)* ')' '=>' term
  | 'fix' identifier ':' type '=' term

type-abstraction ::=
  | '[' identifier (',' identifier)* ']' '=>' term

conditional ::=
  | 'if' term 'then' term 'else' term

type ::=
  | identifier
  | type -> type
  | '[' identifier (',' identifier)* ']' => type
```
