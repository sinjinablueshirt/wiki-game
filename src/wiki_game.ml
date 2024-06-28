open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let helper link_text = 
    match Wikipedia_namespace.namespace link_text with
    | None -> true
    | Some _namespace -> false in 
  let open Soup in
  parse contents
  $$ "a[href*='/wiki/']"
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f: helper
  |> List.dedup_and_sort ~compare:String.compare
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Cat"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
    /wiki/Carnivore
    /wiki/Domestication_of_the_cat
    /wiki/Mammal
    /wiki/Species
    |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

module Article = struct
  type t = {title:string; url: string} [@@deriving compare, sexp, equal, hash]
  let of_string article_string =
    let tokens =  String.split article_string ~on:'/' in
    {title=String.append (List.nth_exn tokens ((List.length tokens) -1)) " - Wikipedia"; url=article_string }

end

module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = Article.t * Article.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

end

let rec _get_connections_from_path_list (article_list : Article.t list) =
  (* gets all the connections aka all possible pairwise combinations of
     elements *)
  match List.length article_list with
  | 0 | 1 -> []
  | 2 ->
    [ ( (List.hd_exn article_list)
      ,(List.hd_exn (List.tl_exn article_list)) )
    ]
  | _ -> [List.hd_exn article_list, List.hd_exn (List.tl_exn article_list)] @ _get_connections_from_path_list (List.tl_exn article_list)
;;

module G = Graph.Imperative.Graph.Concrete (Article)


(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes (article: Article.t) = [ `Shape `Box; `Label article.title; `Fillcolor 1000 ]
    let vertex_name (article: Article.t) = sprintf {|"%s"|} article.title
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

  let correct_url url (how_to_fetch :File_fetcher.How_to_fetch.t) = 
    match how_to_fetch with
    | Local _ -> url
    | Remote -> if not (String.is_prefix ~prefix:"https://en.wikipedia.org/" url) then "https://en.wikipedia.org/" ^ url else url;;


    let get_neighbors_at_level ~current_page ~how_to_fetch = 
      get_linked_articles (File_fetcher.fetch_exn how_to_fetch ~resource:(correct_url current_page how_to_fetch));;


let not_already_visited (article_url:string) visited_set ~how_to_fetch =
    not
      (Hash_set.exists visited_set ~f:(fun article_in_set ->
         String.equal article_in_set (Article.of_string (correct_url article_url how_to_fetch)).title)) (* change to Article.equal *)
  ;;


(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let graph = G.create () in
  let original_article = Article.of_string origin in
  let visited = String.Hash_set.create () in (* strings representing articles titles *)
  let () = Hash_set.add visited origin in
  let rec traverse depth (current_node:Article.t) =
    (match depth=0 with
    | true -> []
    | false -> let neighbors = get_neighbors_at_level ~current_page:current_node.url ~how_to_fetch in
    let connection_neighbors = List.map neighbors ~f:(fun neighbor -> current_node, Article.of_string neighbor) in
    List.iter neighbors ~f:(Hash_set.add visited);
    connection_neighbors@(List.concat_map neighbors ~f:(fun neighbor -> traverse (depth-1) (Article.of_string neighbor))))
  in
    let list_of_connections = traverse max_depth original_article in
    let connection_set = Network.Connection.Set.of_list list_of_connections in
    Set.iter connection_set ~f:(fun (art1, art2) -> G.add_edge graph art1 art2);
    Dot.output_graph (Out_channel.create (File_path.to_string output_file))
    graph;
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;


(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)

  let bfs_find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
    let original_article =  Article.of_string (correct_url origin how_to_fetch) in
    let visited = String.Hash_set.create () in
    let () =  Hash_set.add visited original_article.title in
    let to_visit = Queue.create () in
    Queue.enqueue to_visit [original_article];
    let rec traverse () =
      (match Queue.dequeue to_visit with
      | None -> None
      | Some article_path -> match ((List.length article_path) - max_depth = 0) with
      | true -> None
      | false ->
        let last_article = List.nth_exn article_path ((List.length article_path)-1) in
        let neighbors = get_neighbors_at_level ~current_page:last_article.url ~how_to_fetch in
        let filtered_neighbors = List.filter neighbors ~f:(fun art -> (not_already_visited (correct_url art how_to_fetch) visited ~how_to_fetch))
      in
      List.iter filtered_neighbors ~f:(fun neighbor -> Queue.enqueue to_visit (article_path@[Article.of_string (correct_url neighbor how_to_fetch)]));
        if (String.equal (Article.of_string (correct_url last_article.url how_to_fetch)).title (Article.of_string (correct_url destination how_to_fetch)).title) then Some article_path else (
        Hash_set.add visited last_article.title;
      traverse ();
      )

      ) in
      traverse ()
    

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        (* bfs pre-processing things *)
let start_time = Time_now.nanoseconds_since_unix_epoch () in
match bfs_find_path ~max_depth ~origin ~destination ~how_to_fetch () with
| None -> print_endline "No path found!"
| Some trace ->
  let new_trace = List.map trace ~f:(fun article -> article.url) in
  let end_time = Time_now.nanoseconds_since_unix_epoch () in
  let time_taken = Base.Int63.( - ) end_time start_time in
  print_s
    [%message (new_trace : string list) ~time_taken: (Float.( / ) (Float.of_int63 time_taken) (Float.of_int 1000000000) : Float.t)]]

        (* match bfs_find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> 
          let new_trace = List.map trace ~f:(fun article -> article.url) in
          List.iter new_trace ~f:print_endline] *)
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;

