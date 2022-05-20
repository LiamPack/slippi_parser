open Slippi_parser
open Angstrom

let%test "placeholder" =
  let fn = Util.read_whole_file "raw.slp" in
  match parse_string ~consume:All slippi_raw fn with
  | Ok _      -> true
  | Error msg ->
    print_endline msg;
    false
