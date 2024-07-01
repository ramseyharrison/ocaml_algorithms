(lang dune 2.9)
(name ocaml_algorithms)
(package
 (name ocaml_algorithms)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (alcotest :with-test)
  (ocaml (= "5.0.0"))
  dune)
 (tags
  (topics "to describe" your project)))