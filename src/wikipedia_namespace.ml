open! Core

module Subject = struct
  type without_talk =
    [ `User
    | `Wikipedia
    | `File
    | `Media_wiki
    | `Template
    | `Help
    | `Category
    | `Portal
    | `Draft
    | `Timed_text
    | `Module
    | `Special
    | `Media
    ]
  [@@deriving enumerate]

  type with_talk =
    [ without_talk
    | `Talk
    ]
  [@@deriving enumerate]

  let to_string = function
    | `User -> "User"
    | `Wikipedia -> "Wikipedia"
    | `File -> "File"
    | `Media_wiki -> "MediaWiki"
    | `Template -> "Template"
    | `Help -> "Help"
    | `Category -> "Category"
    | `Portal -> "Portal"
    | `Draft -> "Draft"
    | `Timed_text -> "TimedText"
    | `Module -> "Module"
    | `Special -> "Special"
    | `Media -> "Media"
    | `Talk -> "Talk"
  ;;
end

type t =
  | Subject of Subject.with_talk
  | Talk of Subject.without_talk
[@@deriving enumerate]

let to_string = function
  | Subject s -> Subject.to_string s
  | Talk s -> sprintf !"%{Subject}_talk" s
;;

let namespaces = List.map all ~f:(fun t -> to_string t, t)

let%expect_test "all namespaces" =
  (* Just an expect test showing all of the namespaces we know about. *)
  List.iter namespaces ~f:(fun (s, _) -> print_endline s);
  [%expect
    {|
    User
    Wikipedia
    File
    MediaWiki
    Template
    Help
    Category
    Portal
    Draft
    TimedText
    Module
    Special
    Media
    Talk
    User_talk
    Wikipedia_talk
    File_talk
    MediaWiki_talk
    Template_talk
    Help_talk
    Category_talk
    Portal_talk
    Draft_talk
    TimedText_talk
    Module_talk
    Special_talk
    Media_talk
    |}]
;;

(* This is a bit brittle, but it works well enough for Wikipedia. *)
let namespace s =
  List.find_map namespaces ~f:(fun (namespace, t) ->
    Option.some_if (String.is_substring s ~substring:(namespace ^ ":")) t)
;;
