(lang dune 3.7)

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
   (ocaml (= 5.0.0))
   ocamlformat
   ocaml-lsp-server
   dune
   cmdliner
   ounit2
   core
   iter
   num
   containers
   batteries))
