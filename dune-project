(lang dune 2.0)
(name irmin-statistics)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github liautaud/irmin-statistics))
(maintainers "Romain Liautaud <romain@liautaud.fr>")
(authors "Craig Ferguson <me@craigfe.io>" "Romain Liautaud <romain@liautaud.fr>")

(package
 (name irmin-statistics)
 (synopsis "")
 (description "Computes Irmin storage statistics for a Tezos node.")
 (depends
  (ocaml (>= 4.8.0))
  (irmin (= 2.1.0))
  (tezos-crypto (= 7.1))
  fmt
  logs))
