open OUnit2
open Slippi_parser

let _ =
  "test suite slippi_parser" >::: [ ("placeholder" >:: fun _ -> slippi; assert_equal true true) ]
