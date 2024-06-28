(* Note: This incantation allows us to tell the compiler to temporarily stop
   notifying us when we have unused functions and values. Feel free to delete
   after completing exercise 6. *)
[@@@disable_unused_warnings]

open Core

module Node_id : sig
  (** A [Node_id.t] uniquely identifies a node in a graph. We will using it later for
      looking up and setting the state of nodes in the course of our path search. *)
  type t [@@deriving compare, equal, sexp]

  include Comparable.S with type t := t

  val create : int -> t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  (* Remember that this is the syntax for include modules such as [Map] and
     [Set] that are provided by [Comparable.Make] to our module. In our case,
     we use [Node_id.Map.t] in the [Nodes.t]. *)
  include T
  include Comparable.Make (T)

  let create id = id
end

module Edge = struct
  (** This type represents an edge between two nodes [a] and [b]. Note that since we are
      working with undirected graphs, the order of [a] and [b] do not matter. That is, an
      [Edge.t] { a = 1; b = 2; ... } is equivalent to { a = 2; b = 1; ... } *)

  module T = struct
    type t =
      { a : Node_id.t
      ; b : Node_id.t
      ; distance : int
      }
    [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Edges = struct
  type t = Edge.t list [@@deriving sexp]

  (* Exercise 1: Given a [t] (list of edges) and a [Node_id.t], implement a
     function that returns a list of neighboring nodes with their
     corresponding distances. *)
  let neighbors t node_id : (Node_id.t * int) list =
    List.filter_map t ~f:(fun (edge : Edge.t) ->
      match Node_id.equal node_id edge.a, Node_id.equal node_id edge.b with
      | true, false -> Some (edge.b, edge.distance)
      | false, true -> Some (edge.a, edge.distance)
      | _, _ -> None)
  ;;

  (* We've left all of the tets in this file disabled. As you complete the
     exercises, please make sure to remove `[@tags "disabled"]` and run `dune
     runtest` to ensure that your implementation passes the test. *)
  let%expect_test "neighbors" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      Edge.
        [ { a = n0; b = n1; distance = 1 }
        ; { a = n1; b = n2; distance = 3 }
        ; { a = n2; b = n3; distance = 2 }
        ; { a = n2; b = n4; distance = 1 }
        ; { a = n3; b = n5; distance = 5 }
        ; { a = n4; b = n5; distance = 1 }
        ]
    in
    let neighbors = neighbors t n2 in
    print_s [%message (neighbors : (Node_id.t * int) list)];
    [%expect {| (neighbors ((1 3) (3 2) (4 1))) |}]
  ;;
end

module Node = struct
  module State = struct
    type t =
      | Origin (** Used to mark the node where our search starts *)
      | Unseen (** Used to mark unexplored Nodes *)
      | Todo of
          { distance : int
          ; via : Node_id.t
          }
      (** Used to mark nodes that have been encountered but have not been processed yet *)
      | Done of { via : Node_id.t }
      (** Used to mark nodes that we are finished processing *)
    [@@deriving sexp]
  end

  type t = { mutable state : State.t } [@@deriving fields ~getters, sexp]

  let init () = { state = Unseen }
  let set_state t state = t.state <- state
end

module Nodes = struct
  (** This type represents a stateful collection of nodes in our graph. These [Node.t]s
      will be updated in the course of our graph search to keep track of progress. *)
  type t = Node.t Node_id.Map.t [@@deriving sexp]

  (* Exercise 2: Given a list of edges, create a [t] that contains all nodes
     found in the edge list. Note that you can construct [Node.t]s with the
     [Node.init] function. *)
  let of_edges edges =
    let my_map = Node_id.Map.empty in
    List.iter edges ~f:(fun (edge : Edge.t) ->
      List.iter [ edge.a; edge.b ] ~f:(fun node_id ->
        let node_already_mapped = Map.mem my_map node_id in
        match node_already_mapped with
        | false ->
          let my_map = Map.add my_map ~key:node_id ~data:Node.init in
          ()
        | true -> ()));
    my_map
  ;;

  let find = Map.find_exn
  let state t node_id = find t node_id |> Node.state

  let set_state t id state =
    let node = Map.find_exn t id in
    Node.set_state node state
  ;;

  let is_there_a_todo my_node_list =
    List.exists my_node_list ~f:(fun (id, (state : Node.State.t)) ->
      match state with Todo _ -> true | Unseen -> false | _ -> false)
  ;;

  let get_lowest_todo my_node_list =
    let todo_list =
      List.filter_map my_node_list ~f:(fun (id, (state : Node.State.t)) ->
        match state with
        | Todo { distance; via } -> Some (id, (distance, via))
        | _ -> None)
    in
    List.hd
      (List.sort
         todo_list
         ~compare:(fun (id1, (dist1, via1)) (id2, (dist2, via2)) ->
           if Int.equal dist1 dist2
           then 0
           else if dist1 > dist2
           then 1
           else -1))
  ;;

  (* Exercise 3: Given a [t], find the next node to process by selecting the
     node with the smallest distance along with its via route. *)
  let next_node (t : Node.t Node_id.Map.t)
    : (Node_id.t * (int * Node_id.t)) option
    =
    let unseen_or_todo_map =
      Map.filter t ~f:(fun node ->
        match Node.state node with
        | Origin -> false
        | Unseen -> true
        | Todo _ -> true
        | Done _ -> false)
    in
    let listed_possible_nodes = Map.to_alist unseen_or_todo_map in
    let id_and_state_list =
      List.map listed_possible_nodes ~f:(fun (id, node_obj) ->
        id, Node.state node_obj)
    in
    match List.is_empty id_and_state_list with
    | true -> None
    | false ->
      (match is_there_a_todo id_and_state_list with
       | false ->
         (* return the first node in the list *)
         let id, _ = List.hd_exn id_and_state_list in
         Some (id, (Int.max_value, id))
       | true -> get_lowest_todo id_and_state_list)
  ;;

  let%expect_test "next_node" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Todo { distance = 1; via = n1 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let next_node = next_node t in
    print_s [%message (next_node : (Node_id.t * (int * Node_id.t)) option)];
    [%expect {| (next_node ((4 (1 1)))) |}]
  ;;

  (* Exercise 4: Given a [t] that has already been processed from some origin
     -- that is the origin has been marked as [Origin] and nodes on the
     shortest path have been marked as [Done] -- return the path from the
     origin to the given [distination]. *)
  let path t (destination : Node_id.t) : Node_id.t list =
    let queue = Queue.create () in
    let () = Queue.enqueue queue destination in
    let node_list = [] in
    let filtered_map =
      Map.filter t ~f:(fun node ->
        match Node.state node with
        | Todo _ -> false
        | Unseen -> false
        | _ -> true)
    in
    let rec traverse nodes =
      match Queue.dequeue queue with
      | None -> nodes
      | Some node_id ->
        (match Node.state (Map.find_exn filtered_map node_id) with
         | Origin -> [ node_id ] @ nodes
         | Done { via } ->
           Queue.enqueue queue via;
           traverse [ node_id ] @ nodes
         | _ -> nodes)
    in
    traverse node_list
  ;;

  (* Excercise 5: Write an expect test for the [path] function above. *)
  let%expect_test "path" =
    let n = Node_id.create in
    let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
    let t =
      [ n0, { Node.state = Origin }
      ; n1, { Node.state = Done { via = n0 } }
      ; n2, { Node.state = Done { via = n1 } }
      ; n3, { Node.state = Todo { distance = 2; via = n1 } }
      ; n4, { Node.state = Done { via = n2 } }
      ; n5, { Node.state = Unseen }
      ]
      |> Node_id.Map.of_alist_exn
    in
    let path = path t n4 in
    print_s [%message (path : Node_id.t list)];
    [%expect {| (path (0 1 2 4)) |}]
  ;;
end

(* Exercise 6: Using the functions and types above, implement Dijkstras graph
   search algorithm! Remember to reenable unused warnings by deleting the
   first line of this file. *)
let shortest_path ~edges ~origin ~destination : Node_id.t list = []

let%expect_test ("shortest_path" [@tags "disabled"]) =
  let n = Node_id.create in
  let n0, n1, n2, n3, n4, n5 = n 0, n 1, n 2, n 3, n 4, n 5 in
  let edges =
    Edge.
      [ { a = n0; b = n1; distance = 1 }
      ; { a = n1; b = n2; distance = 1 }
      ; { a = n2; b = n3; distance = 1 }
      ; { a = n2; b = n4; distance = 1 }
      ; { a = n3; b = n5; distance = 2 }
      ; { a = n4; b = n5; distance = 1 }
      ]
  in
  let origin = n0 in
  let destination = n5 in
  let path = shortest_path ~edges ~origin ~destination in
  print_s ([%sexp_of: Node_id.t list] path);
  [%expect {| (0 1 2 4 5) |}]
;;

(* Exercise 7: Add some more test cases, exploring any corner cases you can
   think of. *)
