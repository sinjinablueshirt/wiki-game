open! Core

(** This module helps us handle Wikipedia namespaces
    (https://en.wikipedia.org/wiki/Wikipedia:Namespace).

    Note that in the [Subject] module, we use polymorphic variants
    (https://v2.ocaml.org/manual/polyvariant.html). This lets us easily separate out the
    list of all of the subjects without the "Talk" subject without having to list all of
    them again them when we want to enumerate all subjects including the "Talk"
    subject.

    The reason we want this separation is because there is no special "Talk" namespace for
    the "Talk" subject, which is exclusively used for articles. For more information, see:
    https://en.wikipedia.org/wiki/Wikipedia:Namespace#Talk_namespaces *)
module Subject : sig
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

  type with_talk =
    [ without_talk
    | `Talk
    ]
end

type t =
  | Subject of Subject.with_talk
  | Talk of Subject.without_talk

(** [namespace url] looks at [url] to see whether it falls within a namespace, returning
    the namespace if it exists, and [None] otherwise.

    For example, calling [namespace] on
    "https://en.wikipedia.org/wiki/Special:AllPages" will return [Some Special],
    while calling [namespace] on "https://en.wikipedia.org/wiki/Cat" will return [None].

    [namespace] does not employ particularly smart parsing logic, and should be able to
    handle both fully specified urls (like "https://en.wikipedia.org/wiki/Cat") and
    relative urls (like "/wiki/Cat"). *)
val namespace : string -> t option
