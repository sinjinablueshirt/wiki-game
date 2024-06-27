open! Core

module Position = struct
  type t = int * int [@@deriving compare, sexp, equal, hash]

  let to_string (row, col) =
    "(" ^ Int.to_string row ^ ", " ^ Int.to_string col ^ ")"
  ;;
end

let create_grid input_file =
  (* returns 2-d array of board*)
  let line_list = In_channel.read_lines (File_path.to_string input_file) in
  List.map line_list ~f:(fun line -> String.to_list line)
;;

let get_char_in_a_row row_list char =
  (* given a row, return a list of all the indices where there is the
     character passed in *)
  List.filter_mapi row_list ~f:(fun index symbol ->
    if Char.equal symbol char then Some index else None)
;;

let get_empty_spaces (grid : char list list) =
  (* given a grid, return a list of tuples of all valid spaces to move to *)
  List.concat
    (List.mapi grid ~f:(fun index row ->
       List.map (get_char_in_a_row row '.') ~f:(fun col -> index, col)))
;;

let get_starting_position (grid : char list list) =
  (* given a grid, return a list of tuples of all valid spaces to move to *)
  List.concat
    (List.mapi grid ~f:(fun index row ->
       List.map (get_char_in_a_row row 'S') ~f:(fun col -> index, col)))
;;

let get_ending_position (grid : char list list) =
  (* given a grid, return a list of tuples of all valid spaces to move to *)
  List.concat
    (List.mapi grid ~f:(fun index row ->
       List.map (get_char_in_a_row row 'E') ~f:(fun col -> index, col)))
;;

let step_from_a_position (row, col) =
  (* returns a list of tuples representing the four possible directions that
     one can go from that position(even if not a valid move or out of
     bounds!)*)
  [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
;;

let solve_maze input_file =
  let grid = create_grid input_file in
  let empty_spaces = get_empty_spaces grid in
  let starting_position = List.hd_exn (get_starting_position grid) in
  let ending_postion = List.hd_exn (get_ending_position grid) in
  let checked = Hash_set.create (module Position) in
  let rec recursive_function current_position (so_far : Position.t list) =
    let possible_moves = step_from_a_position current_position in
    let valid_moves =
      List.filter possible_moves ~f:(fun p1 ->
        List.exists
          ([ starting_position ] @ empty_spaces @ [ ending_postion ])
          ~f:(fun p2 -> Position.equal p1 p2))
    in
    let filtered_valid_moves =
      List.filter valid_moves ~f:(fun mov ->
        not (Hash_set.exists checked ~f:(fun pos -> Position.equal pos mov)))
    in
    let () =
      List.iter filtered_valid_moves ~f:(fun move ->
        Hash_set.add checked move)
    in
    match List.length filtered_valid_moves with
    | 0 -> None
    | _ ->
      List.find_map filtered_valid_moves ~f:(fun next ->
        if Position.equal next ending_postion
        then Some (so_far @ [ next ])
        else (
          match recursive_function next (so_far @ [ next ]) with
          | None -> None
          | Some list_of_coords -> Some list_of_coords))
  in
  let () = Hash_set.add checked starting_position in
  recursive_function starting_position []
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
        let solution_path = solve_maze input_file in
        match solution_path with
        | None -> print_endline "No path found!"
        | Some path ->
          let stringified = List.map path ~f:Position.to_string in
          List.iter stringified ~f:print_endline]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
