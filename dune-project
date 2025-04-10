(lang dune 3.11)
(name dbl)

(generate_opam_files true)

(source (github fram-lang/dbl))
(license MIT)

(package
  (name dbl)
  (synopsis "Interpreter of Fram: a language with algebraic effects and powerful named parameters")
  (description "")
  (depends
    (dune (>= 3.11))
    (dune-glob (>= 3.7.0))))
