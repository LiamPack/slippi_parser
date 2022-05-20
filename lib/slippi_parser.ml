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
  ; dashback_fix : int list (* int8 * 4 in one 32bit value *)
  ; shield_drop_fix : int list (* int8 * 4 in one 32bit value *)
  ; nametags : string list (* just a string list for now *)
  ; pal : int
  ; frozen_ps : int
  ; minor_scene : int (* u8, always 0x2 *)
  ; major_scene : int (* u8, 0x2 in VS mode, 0x8 online *)
  ; display_names : string list (* just a big string for now *)
  ; connect_codes : string list
  ; slippi_uids : string list option
  ; language_option : int option (* u8, 0 for jp, 1 for en *)
  }

type pre_frame_update =
  { frame_number : int
  ; player_index : int
  ; is_follower : int (* bool-ish *)
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

type post_frame_update =
  { frame_number : int
  ; player_index : int (* 32 *)
  ; is_follower : int (* bool-ish *)
  ; internal_character_id : int
  ; action_state_id : int (* 16 *)
  ; x_position : float
  ; y_position : float
  ; facing_direction : float
  ; percent : float
  ; shield_size : float
  ; last_hitting_attack_id : int
  ; current_combo_count : int
  ; last_hit_by : int
  ; stocks_remaining : int
  ; action_state_frame_counter : float
  ; state_bit_flags_1 : int
  ; state_bit_flags_2 : int
  ; state_bit_flags_3 : int
  ; state_bit_flags_4 : int
  ; state_bit_flags_5 : int
  ; misc_as : float
  ; ground_air_state : int (* bool-ish *)
  ; last_ground_id : int (* 16 *)
  ; jumps_remaining : int
  ; l_cancel_status : int (* 0 = none, 1 = success, 2 = unsuccess *)
  ; hurtbox_collision_state : int (* 0 = vuln, 1 = invuln, 2 = intang *)
  ; self_induced_air_x_speed : float
  ; self_induced_air_y_speed : float
  ; attack_based_x_speed : float
  ; attack_based_y_speed : float
  ; self_induced_ground_x_speed : float
  ; hitlag_frames_remaining : float (* 0 = "not in hitlag" *)
  ; animation_index : int option (* 32 !!! 3.11.0 *)
  }

type game_end =
  { game_end_method : int
  ; lras_initiator : int
  }

type frame_start =
  { frame_number : int
  ; random_seed : int
  ; scene_frame_counter : int option
  }

type item_update = unit

type frame_bookend =
  { frame_number : int
  ; latest_finalized_frame : int
  }

type event =
  | GameStart       of game_start
  | PreFrameUpdate  of pre_frame_update
  | PostFrameUpdate of post_frame_update
  | GameEnd         of game_end
  | FrameStart      of frame_start
  | ItemUpdate      of item_update
  | FrameBookend    of frame_bookend
  | Unimplemented   of string
(* | GeckoList       of gecko_list *)

let peek1 = peek_char_fail >>| fun x -> x

let take1 = take 1 >>| fun x -> String.get x 0

let any_int_of_int32 =
  let open BE in
  any_int32 >>| Int32.to_int


let print_unimplemented e =
  match e with
  | Ok e ->
    List.map
      (fun r ->
        match r with
        | Unimplemented s ->
          print_endline s;
          print_endline "";
          print_endline ""
        | _               -> print_endline "clean")
      e
  | _    -> [ print_endline "idk" ]


module E = struct
  module CharMap = Map.Make (Char)

  let event_payload_size = char '\053' *> any_uint8

  let other_event_payload = take1 >>= fun cb -> BE.any_uint16 >>| fun ps -> cb, ps

  let payload_sizes_map n =
    count n other_event_payload
    >>| fun s -> List.fold_left (fun m (k, v) -> CharMap.add k v m) CharMap.empty s


  let all_payload_sizes =
    event_payload_size >>= fun payload_size -> payload_sizes_map ((payload_size - 1) / 3)


  let all_payload_sizes_record =
    event_payload_size
    >>= fun payload_size -> count ((payload_size - 1) / 3) other_event_payload
end

module G = struct
  let version =
    count 4 any_uint8
    >>= function
    | [ major; minor; build; unused ] -> return { major; minor; build; unused }
    | _ -> fail "_version not equal to 4 pieces"


  let game_info_block = take 312

  let parse_slippi_uids v =
    if v.major >= 3 && v.minor >= 11
    then count 4 (take 29) >>| fun x -> Some x
    else return None


  let parse_language_option v =
    if v.major >= 3 && v.minor >= 11 then any_uint8 >>| fun x -> Some x else return None


  let game_start =
    version
    >>= fun version ->
    game_info_block
    >>= fun game_block_info ->
    any_int_of_int32
    >>= fun random_seed ->
    count 4 any_int_of_int32
    >>= fun dashback_fix ->
    count 4 any_int_of_int32
    >>= fun shield_drop_fix ->
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
    count 4 (take 31)
    >>= fun display_names ->
    count 4 (take 10)
    >>= fun connect_codes ->
    parse_slippi_uids version
    >>= fun slippi_uids ->
    parse_language_option version
    >>| fun language_option ->
    GameStart
      { version
      ; game_block_info
      ; random_seed
      ; dashback_fix
      ; shield_drop_fix
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


  let game_end =
    any_uint8
    >>= fun game_end_method ->
    any_int8 >>| fun lras_initiator -> GameEnd { game_end_method; lras_initiator }
end

module Frames = struct
  let parse_scene_frame_counter v =
    if v.major >= 3 && v.minor >= 10
    then any_int_of_int32 >>| fun x -> Some x
    else return None


  let frame_start =
    any_int_of_int32
    >>= fun frame_number ->
    any_int_of_int32
    >>= fun random_seed ->
    parse_scene_frame_counter { major = 3; minor = 9; build = 0; unused = 0 }
    >>| fun scene_frame_counter ->
    FrameStart { frame_number; random_seed; scene_frame_counter }


  let frame_bookend =
    any_int_of_int32
    >>= fun frame_number ->
    any_int_of_int32
    >>| function
    | latest_finalized_frame -> FrameBookend { frame_number; latest_finalized_frame }


  let pre_frame_update =
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


  let parse_animation_index v =
    if v.major >= 3 && v.minor >= 11
    then any_int_of_int32 >>| fun x -> Some x
    else return None


  let post_frame_update =
    let open BE in
    any_int_of_int32
    >>= fun frame_number ->
    any_uint8
    >>= fun player_index ->
    any_uint8
    >>= fun is_follower ->
    any_uint8
    >>= fun internal_character_id ->
    any_uint16
    >>= fun action_state_id ->
    any_float
    >>= fun x_position ->
    any_float
    >>= fun y_position ->
    any_float
    >>= fun facing_direction ->
    any_float
    >>= fun percent ->
    any_float
    >>= fun shield_size ->
    any_uint8
    >>= fun last_hitting_attack_id ->
    any_uint8
    >>= fun current_combo_count ->
    any_uint8
    >>= fun last_hit_by ->
    any_uint8
    >>= fun stocks_remaining ->
    any_float
    >>= fun action_state_frame_counter ->
    any_uint8
    >>= fun state_bit_flags_1 ->
    any_uint8
    >>= fun state_bit_flags_2 ->
    any_uint8
    >>= fun state_bit_flags_3 ->
    any_uint8
    >>= fun state_bit_flags_4 ->
    any_uint8
    >>= fun state_bit_flags_5 ->
    any_float
    >>= fun misc_as ->
    any_uint8
    >>= fun ground_air_state ->
    any_uint16
    >>= fun last_ground_id ->
    any_uint8
    >>= fun jumps_remaining ->
    any_uint8
    >>= fun l_cancel_status ->
    any_uint8
    >>= fun hurtbox_collision_state ->
    any_float
    >>= fun self_induced_air_x_speed ->
    any_float
    >>= fun self_induced_air_y_speed ->
    any_float
    >>= fun attack_based_x_speed ->
    any_float
    >>= fun attack_based_y_speed ->
    any_float
    >>= fun self_induced_ground_x_speed ->
    any_float
    >>= fun hitlag_frames_remaining ->
    parse_animation_index { major = 3; minor = 9; build = 0; unused = 0 }
    >>| fun animation_index ->
    PostFrameUpdate
      { frame_number : int
      ; player_index : int (* 32 *)
      ; is_follower : int (* bool-ish *)
      ; internal_character_id : int
      ; action_state_id : int (* 16 *)
      ; x_position : float
      ; y_position : float
      ; facing_direction : float
      ; percent : float
      ; shield_size : float
      ; last_hitting_attack_id : int
      ; current_combo_count : int
      ; last_hit_by : int
      ; stocks_remaining : int
      ; action_state_frame_counter : float
      ; state_bit_flags_1 : int
      ; state_bit_flags_2 : int
      ; state_bit_flags_3 : int
      ; state_bit_flags_4 : int
      ; state_bit_flags_5 : int
      ; misc_as : float
      ; ground_air_state : int (* bool-ish *)
      ; last_ground_id : int (* 16 *)
      ; jumps_remaining : int
      ; l_cancel_status : int (* 0 = none, 1 = success, 2 = unsuccess *)
      ; hurtbox_collision_state : int (* 0 = vuln, 1 = invuln, 2 = intang *)
      ; self_induced_air_x_speed : float
      ; self_induced_air_y_speed : float
      ; attack_based_x_speed : float
      ; attack_based_y_speed : float
      ; self_induced_ground_x_speed : float
      ; hitlag_frames_remaining : float (* 0 = "not in hitlag" *)
      ; animation_index : int option (* 32 !!! 3.11.0 *)
      }
end

module Util = struct
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s


  (* From https://www.systutorials.com/how-to-measure-a-functions-execution-time-in-ocaml/ *)
  let time f =
    let t = Unix.gettimeofday () in
    let res = f () in
    Printf.printf "Execution time: %f secondsn" (Unix.gettimeofday () -. t);
    res
end

let _event : char -> event t = function
  (* | '\053' -> return 0 *)
  | '\054' -> G.game_start
  | '\055' -> Frames.pre_frame_update
  | '\056' -> Frames.post_frame_update
  | '\057' -> G.game_end
  | '\058' -> Frames.frame_start
  (* | '\059' -> item_update *)
  | '\060' -> Frames.frame_bookend
  (* | '\061' -> gecko_list *)
  (* | '\016' -> partial_message *)
  | _ -> fail "Event not yet supported"


let slippi =
  let f =
    E.all_payload_sizes
    >>= function
    | payload_map ->
      let event =
        take1
        >>= fun c ->
        _event c <|> (take (E.CharMap.find c payload_map) >>| fun s -> Unimplemented s)
      in
      many event
  in
  f <?> "Slippi"
