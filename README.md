# LambdaMangoUwU

This is an interpreter for the Untyped Lambda Calculus and (if wanted)
System F-Omega. The typed mode can (in the future lol) be enabled at
your option.

## Features

- Untyped Lambda Calculus (base).
- Extension that allows native naturals.
- Extension that allows native booleans.

## Untyped Lambda Calculus

The
[Untyped Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
is the base of the interpreter. It's a minimalistic and simple way to
represent computation.

### Native Naturals

The interpreter allows for native naturals, so you can define and use
them using standard digits. Digits are not enabled when this extension
is disabled.

### Both native

They provide function to operate over those native types. When applied
with other abstractions, they simply cannot Beta-reduce.

### Grammar

The informal grammar of this language is as follows:

```
Expression :=
  | TermVariable -- Variable (initial lowercase).
  | Lambda TermVariable "->" Expression -- Abstraction.
  | Expression Expression -- Application.
  | "(" Expression ")"

Declaration := 
  | "let" TermVariable ":=" Expression "." Declaration*
  | EndOfInput
```

Expression application is left-associative and has the highest
precedence. Parentheses are used to indicate precedence, and
abstraction binds the most.

### Examples
As prompt: `id id id id someFreeVar`
As context:
```
let id := \x -> x.
```
The prompt only allows Expressions, and the context only allows
Declarations.

## Running

Only the frontend web (Scala JS) is working. That is:

```
mill -w lambda.fullLinkJS
# or
mill -w lambda.fastLinkJS
```

But tests run on the JVM:

```
mill -w lambdaCoreJVM.tests
```
We should have a JVM/LLVM frontend in the near future.

## Future work

- System F-Omega. 