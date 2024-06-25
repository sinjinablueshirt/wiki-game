open! Core
module City = String

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = City.t * City.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    let pair_first city_list =
      (* pair the first element of the list with all other elem in list.
         returns a list of tuples *)
      let first = City.of_string (List.hd_exn city_list) in
      let rest = List.tl_exn city_list in
      let tuplize elem = first, City.of_string elem in
      List.map rest ~f:tuplize
    ;;

    let rec get_connections city_list =
      (* gets all the connections aka all possible pairwise combinations of
         elements *)
      match List.length city_list with
      | 0 | 1 -> []
      | 2 ->
        [ ( City.of_string (List.hd_exn city_list)
          , City.of_string (List.hd_exn (List.tl_exn city_list)) )
        ]
      | _ -> pair_first city_list @ get_connections (List.tl_exn city_list)
    ;;

    let parse_line s =
      let cities = String.split s ~on:',' in
      get_connections (List.tl_exn cities)
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let parse_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.map
           ~f:Connection.parse_line (* now have a list of lists of tuples*)
      |> List.map ~f:(List.concat_map ~f:(fun (a, b) -> [ a, b; b, a ]))
      |> List.concat
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.parse_file input_file in
        printf !"%{sexp: Network.t}\n" network]
;;

(* ignore (input_file : File_path.t); failwith "TODO"] *)
module G = Graph.Imperative.Graph.Concrete (City)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = sprintf {|"%s"|} v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.parse_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, city2) ->
          G.add_edge graph city1 city2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
