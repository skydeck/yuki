open Riak

module type Conn = sig
  val with_connection : (riak_connection -> 'a Lwt.t) -> 'a Lwt.t
end

module type Elem = sig
  type t
  val of_string : string -> t
  val to_string : t -> string

  val bucket : string
  (*val to_key : t -> string*)
end

module type Ord = sig
  include Elem
  val compare : t -> t -> int
end
