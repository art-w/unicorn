(lang dune 2.8)

(generate_opam_files true)

(name unicorn)

(source
 (github art-w/unicorn))

(license MIT)

(authors "Arthur Wendling")

(maintainers "art.wendling@gmail.com")

(version 0.1)

(package
 (name unicorn_jsoo)
 (synopsis "Purely functional GUI library (js_of_ocaml backend)")
 (depends
  (ocaml
   (>= "4.12.0"))
  (js_of_ocaml
   (>= "6.0.0"))
  optic))

(package
 (name optic)
 (synopsis "Isomorphisms, lenses and prisms")
 (depends
  (ocaml
   (>= "4.08"))))

(package
 (name ppx_deriving_optic)
 (synopsis "PPX deriver for lenses and prisms")
 (depends
  (ocaml
   (>= "4.08"))
  (ppxlib
   (>= "0.35.0"))
  (ppx_deriving
   (>= "6.0.0"))))
