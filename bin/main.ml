open Slippi_parser
open Angstrom
open Batteries

let get_parsed () =
  let fn = Util.read_whole_file "../test/raw.slp" in
  match parse_string ~consume:All slippi_raw fn with
  | Ok c      -> c
  | Error msg -> failwith msg


let get_distances slp =
  let postframes =
    BatList.filter_map
      (fun x ->
        match x with
        | PostFrameUpdate c -> if c.frame_number >= 0 then Some c else None
        | _                 -> None)
      slp
  in
  let grouped_by_player =
    BatList.group
      (fun (x : post_frame_update) (y : post_frame_update) ->
        x.player_index - y.player_index)
      postframes
  in
  let grouped_by_frame =
    BatList.combine (BatList.first grouped_by_player) (BatList.nth grouped_by_player 1)
  in
  let sqr x = Float.pow x 2. in
  let distances =
    BatList.map
      (fun ((p : post_frame_update), (q : post_frame_update)) ->
        sqrt
        @@ (sqr (p.x_position -. q.x_position) +. (sqr @@ (p.y_position -. q.y_position))))
      grouped_by_frame
  in
  distances


let close_distances d distances =
  let closer = BatList.group_consecutive (fun x y -> x < d && y < d) distances in
  BatList.filteri_map
    (fun i x -> if BatList.length x > 10 then Some (i, BatList.length x) else None)
    closer


let _ = close_distances 10. @@ get_distances @@ get_parsed ()
