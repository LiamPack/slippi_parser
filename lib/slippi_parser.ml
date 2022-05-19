open Angstrom

type version =
  { major : int
  ; minor : int
  ; build : int
  ; unused : int
  }

type game_start =
  { version : version
  ; game_block_info : string
  ; random_seed : int (* int32 *)
  ; dashback_fix : int list (* int32 * 4 *)
  ; nametags : string list (* just a string list for now *)
  ; pal : int
  ; frozen_ps : int
  ; minor_scene : int (* u8, always 0x2 *)
  ; major_scene : int (* u8, 0x2 in VS mode, 0x8 online *)
  ; display_names : string list (* just a big string for now *)
  ; connect_codes : string list
  ; slippi_uids : string list (* just a big string for now *)
  ; language_option : int (* u8, 0 for jp, 1 for en *)
  }

type pre_frame_update =
  { frame_number : int
  ; player_index : int
  ; is_follower : int
  ; random_seed : int
  ; action_state_id : int
  ; x_position : float
  ; y_position : float
  ; facing_direction : float
  ; joystick_x : float
  ; joystick_y : float
  ; c_stick_x : float
  ; c_stick_y : float
  ; trigger : float
  ; processed_buttons : int
  ; physical_buttons : int
  ; physical_l_trigger : float
  ; physical_r_trigger : float
  ; x_analog_ucf : int
  ; percent : float
  }

type game_end =
  { game_end_method : int
  ; lras_initiator : int
  }

type frame_start =
  { frame_number : int
  ; random_seed : int
  ; scene_frame_counter : int
  }

type item_update = unit

type frame_bookend =
  { frame_number : int
  ; latest_finalized_frame : int
  }

type event =
  | GameStart       of game_start
  | PreFrameUpdate  of pre_frame_update
  | PostFrameUpdate of unit
  | GameEnd         of game_end
  | FrameStart      of frame_start
  | ItemUpdate      of item_update
  | FrameBookend    of frame_bookend
(* | GeckoList       of gecko_list *)

let peek1 = peek_char_fail >>| fun x -> x

let take1 = take 1 >>| fun x -> String.get x 0

let any_int_of_int32 =
  let open BE in
  any_int32 >>| Int32.to_int


module E = struct
  module IntMap = Map.Make (Int)

  let _event_payload_size : int t = char '\053' *> any_uint8 >>| fun ps -> ps

  let _other_event_payload = any_uint8 >>= fun cb -> BE.any_uint16 >>| fun ps -> cb, ps

  let _payload_sizes_map n =
    count n _other_event_payload
    >>| fun s -> List.fold_left (fun m (k, v) -> IntMap.add k v m) IntMap.empty s


  let _all_payload_sizes =
    _event_payload_size
    >>= fun payload_size -> _payload_sizes_map ((payload_size - 1) / 3) >>| fun m -> m


  let _all_payload_sizes_record =
    _event_payload_size
    >>= fun payload_size ->
    count ((payload_size - 1) / 3) _other_event_payload >>| fun m -> m
end

module G = struct
  let _version =
    count 4 any_uint8
    >>= function
    | [ a; b; c; d ] -> return { major = a; minor = b; build = c; unused = d }
    | _              -> fail "_version more than 4 versions"


  let _game_info_block = take 312

  let _game_start =
    let open BE in
    _version
    >>= fun version ->
    _game_info_block
    >>= fun game_block_info ->
    any_int_of_int32
    >>= fun random_seed ->
    count 4 any_int_of_int32
    >>= fun dashback_fix ->
    count 4 (take 16)
    >>= fun nametags ->
    any_uint8
    >>= fun pal ->
    any_uint8
    >>= fun frozen_ps ->
    any_uint8
    >>= fun minor_scene ->
    any_uint8
    >>= fun major_scene ->
    count 4 (take 16)
    >>= fun display_names ->
    count 4 (take 10)
    >>= fun connect_codes ->
    count 4 (take 29)
    >>= fun slippi_uids ->
    any_uint8
    >>| fun language_option ->
    GameStart
      { version
      ; game_block_info
      ; random_seed
      ; dashback_fix
      ; nametags
      ; pal
      ; frozen_ps
      ; minor_scene
      ; major_scene
      ; display_names
      ; connect_codes
      ; slippi_uids
      ; language_option
      }


  let _game_end =
    any_uint8
    >>= fun game_end_method ->
    any_int8 >>| fun lras_initiator -> GameEnd { game_end_method; lras_initiator }
end

module Frames = struct
  let _frame_start =
    let open BE in
    any_int_of_int32
    >>= fun frame_number ->
    any_int_of_int32
    >>= fun random_seed ->
    any_int_of_int32
    >>| fun scene_frame_counter ->
    FrameStart { frame_number; random_seed; scene_frame_counter }


  let _frame_bookend =
    let open BE in
    any_int_of_int32
    >>= fun frame_number ->
    any_int_of_int32
    >>| function
    | latest_finalized_frame -> FrameBookend { frame_number; latest_finalized_frame }


  let _pre_frame_update =
    let open BE in
    any_int_of_int32
    >>= fun frame_number ->
    any_int8
    >>= fun player_index ->
    any_int8
    >>= fun is_follower ->
    any_int_of_int32
    >>= fun random_seed ->
    any_int16
    >>= fun action_state_id ->
    any_float
    >>= fun x_position ->
    any_float
    >>= fun y_position ->
    any_float
    >>= fun facing_direction ->
    any_float
    >>= fun joystick_x ->
    any_float
    >>= fun joystick_y ->
    any_float
    >>= fun c_stick_x ->
    any_float
    >>= fun c_stick_y ->
    any_float
    >>= fun trigger ->
    any_int_of_int32
    >>= fun processed_buttons ->
    any_int16
    >>= fun physical_buttons ->
    any_float
    >>= fun physical_l_trigger ->
    any_float
    >>= fun physical_r_trigger ->
    any_int8
    >>= fun x_analog_ucf ->
    any_float
    >>| fun percent ->
    PreFrameUpdate
      { frame_number
      ; player_index
      ; is_follower
      ; random_seed
      ; action_state_id
      ; x_position
      ; y_position
      ; facing_direction
      ; joystick_x
      ; joystick_y
      ; c_stick_x
      ; c_stick_y
      ; trigger
      ; processed_buttons
      ; physical_buttons
      ; physical_l_trigger
      ; physical_r_trigger
      ; x_analog_ucf
      ; percent
      }
end

module Util = struct
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
end

let _event : char -> event t = function
  (* | '\053' -> return 0 *)
  | '\054' -> G._game_start
  | '\055' -> Frames._pre_frame_update
  (* | '\056' -> Frames._post_frame_update *)
  | '\057' -> G._game_end
  | '\058' -> Frames._frame_start
  (* | '\059' -> _item_update *)
  | '\060' -> Frames._frame_bookend
  (* | '\061' -> _gecko list *)
  (* | '\010' -> _partial_message *)
  | _ -> fail "Event not yet supported"


let slippi =
  let f _ =
    peek_char_fail
    >>= function
    | c ->
      (match c with
      | '{' -> take 15 *> E._all_payload_sizes_record
      | _   -> E._all_payload_sizes_record)
  in
  fix f <?> "slippi"
