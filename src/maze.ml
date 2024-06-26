open! Core

module Coordinate = struct
  type t = int * int [@@deriving compare, sexp, equal, hash]
end

module Game = struct
  type t =
    { mutable pointer_position : Coordinate.t
    ; all_valid_spaces : Coordinate.t Hash_set.t
    }
end

let create_grid input_file =
  (* returns 2-d array of board*)
  let line_list = In_channel.read_lines (File_path.to_string input_file) in
  List.map line_list ~f:(fun line -> String.split line)
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        ignore (input_file : File_path.t);
        failwith "TODO"]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
