(lang dune 3.3)

(name atcoder)

(generate_opam_files true)

(source (github lmdexpr/contest))

(authors "Yuki Tajiri")

(maintainers "Yuki Tajiri <lmdexpr@gmail.com>")

(license MIT)

(package
 (name atcoder)
 (synopsis "For contest.")
 (allow_empty)
 (depends
   (ocaml (= 4.10.2))
   ocamlformat
   ocaml-lsp-server
   dune
   cmdliner
   ounit2
   core
   iter
   num))
