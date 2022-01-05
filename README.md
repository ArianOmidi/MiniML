# MiniML


<!-- ABOUT THE PROJECT -->
## About

A miniature implementation of the OCaml language, with simple code analysis tools, an interpreter (which evaluates an expression), a type checker, and a type inference engine.

### Features:
* Code Analysis Tools
* Parser
* Substitution
* Big Step Evaluation
* Type Checking
* Unification
* Type Inference

<!-- GETTING STARTED -->
## Getting Started

The MiniML internal language can be described as the following

```
types T := int | bool |  T1 ->  T2 |  T1 * ... * Tn
expressions e := ... | fn x : T => e | e1 e2 | rec f : T => e | (e1, ... , e2) | let ds in e end
declaration d := val x = e | val (x1, ... , xn) = e | name x = e
declarations ds :=  . | d ds
```
