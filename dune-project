(lang dune 3.20)

(name contest)

(generate_opam_files true)

(source (github lmdexpr/contest))

(authors "Yuki Tajiri")

(maintainers "Yuki Tajiri <lmdexpr@gmail.com>")

(license MIT)

(package
 (name contest)
 (synopsis "For contest.")
 (allow_empty)
 (depends
   (ocaml (= 5.3.0))
   ocamlformat
   ocaml-lsp-server
   dune
   ounit2
   (core (= v0.17.1))
   (iter (= 1.9))
   (num (= 1.6))
   (containers (= 3.16))
   (batteries (= 3.9.0))
 ))
