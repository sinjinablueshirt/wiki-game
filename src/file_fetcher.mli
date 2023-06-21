open! Core

(** [File_fetcher] is a wrapper around logic for loading files. It knows how to make HTTP
    requests, and also exposes a way to load files locally for ease of testing.

    [File_fetcher] is a bit particular about the way resources are specified. If [Remote],
    all resources should be fully specified (including domain). If [Local], all resources
    should be specified with using the provided root.

    For example, if fetching "/wiki/Cat" from Wikipedia, [resource] must be fully
    specified as "en.wikipedia.org/wiki/Cat". If fetching the file at
    "/home/pictures/cat.png" with root specified as [Local /home], the resource should be
    specified as "/pictures/cat.png". *)

module How_to_fetch : sig
  type t =
    | Local of File_path.t
    | Remote

  val param : t Command.Param.t
end

module Resource = String

(** We expose a command parameter that allows users to specify both the resource to fetch
    and how to fetch it. See file_fetcher_demo.ml for an example. *)
val param : (How_to_fetch.t * Resource.t) Command.Param.t

(** [fetch_exn] will raise an exception if [resource] is unreadable (either the resource
    does not exist/is not readable when reading locally, or we cannot successfully make
    the HTTP request when fetching remotely). *)
val fetch_exn : How_to_fetch.t -> resource:string -> string
