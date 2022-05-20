(lang dune 3.0)

(name slippi_parser)

(generate_opam_files true)

(source
 (github packell1/slippi_parser))

(authors "Liam Packer")

(maintainers "Liam Packer")

(license LICENSE)

(package
 (name slippi_parser)
 (synopsis "A parser for the `.slp` file format")
 (description "A parser for the `.slp` file format. While `.slp` files adhere to the UBJSON spec (see https://github.com/project-slippi/slippi-wiki/blob/master/SPEC.md#ubjson), the `raw` element contains packaged information in a specific format described only by the `.slp` spec.")
 (depends ocaml dune angstrom ppx_inline_test unix)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
