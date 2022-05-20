open OUnit2
(* open Slippi_parser *)
(* open Angstrom *)

let _ =
  "test suite slippi_parser" >::: [ ("placeholder" >:: fun _ -> assert_equal true true) ]
