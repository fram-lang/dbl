DBL
===

DBL is an interpreter of Fram, an experimental programming language designed
around the idea of combining lexically scoped effect handlers with a powerful
mechanism of implicit and named parameters in a strongly-typed setting. The
main goal of DBL interpreter is providing a tool for bootstrapping
[Fram compiler](https://github.com/fram-lang/fram). DBL implements core and
some of advanced language features, including ML-style parametric polymorphism,
rank-N types, mutually recursive data-types and definitions, existential types,
and pattern-matching.

Requirements
------------

DBL is written in OCaml and tested with OCaml system version 5.2.0. DBL uses
dune as a build system (tested with version 3.17.2). This project requires
`dune-glob` package, which is a separate package from the Dune project that
must be installed explicitly (e.g., via opam). 

Installation
------------

Simply type `dune build` to compile the project. If you use opam package
manager, you can locally install DBL by typing `dune install`.

Usage
-----

Just type `dbl` (or `rlwrap dbl` for better readline support) to run the
interpreter in a REPL mode. In this mode you can interactively provide
definitions and expressions to evaluate, separated by double semicolon `;;`.
The example REPL session is shown below.
```
$ dbl
> let id x = x ;;
> id () ;;
: Unit
= ()
> 
```
You can also run programs in a batch mode, by providing a file to execute
as a command-line argument (e.g., `dbl filename.fram`).

If you didn't install DBL via `dune install` (or OPAM, generally) then
it's recommended to set the `DBL_LIB` environmental variable to the `lib`
directory of your local installation of DBL.

Examples
--------

Several simple examples can be found in the `examples` directory. Moreover,
in a `test/ok` directory you can find many tiny examples used to test
implementation of various language features.

Source Code and Development
---------------------------

Source code can be found in the `src` directory. For a high-level overview
of the implementation we encourage to look into the `src/Pipeline.ml` file
and `src/Lang` directory to see phases and intermediate languages of the
interpreter. To run DBL interpreter on all test programs just type
`./test.sh dbl test/test_suite`. Bugs can be reported on our
[issue tracker](https://github.com/fram-lang/dbl/issues).
