(lang dune 3.00)
(name rincewind)

(authors "Hervé Giraud <giraud.contact@yahoo.fr>")
(maintainers "Hervé Giraud <giraud.contact@yahoo.fr>")
(license MIT)
(source (github giraud/rincewind))

(generate_opam_files true)
(package
  (name rincewind)
  (synopsis "Simple Cmt information extractor")
  (depends ocaml dune cppo ocamlformat)
)

