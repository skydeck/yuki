open Riak
open Batteries

module Make(Conn:Make.Conn)(Elem:Make.Elem) : sig
  module Usermeta : sig
    type t = (string, string) Map.t
  end

  type t = {
    key : riak_key;
    value : Elem.t;
    links : riak_key list;
    usermeta : Usermeta.t;
  }

  val get : riak_key -> t Lwt.t
  val put : ?key:riak_key -> ?usermeta:Usermeta.t -> ?ops:riak_put_option list -> Elem.t -> riak_key list -> riak_key Lwt.t

  val read : riak_key -> (Elem.t -> 'a Lwt.t) -> 'a Lwt.t
  val read_default : riak_key -> Elem.t -> (Elem.t -> 'a Lwt.t) -> 'a Lwt.t

  val write : riak_key -> (Elem.t -> Elem.t Lwt.t) -> riak_key Lwt.t
  val write_default : riak_key -> Elem.t -> (Elem.t -> Elem.t Lwt.t) -> unit Lwt.t
  val write' : riak_key -> (Elem.t -> ('a * Elem.t) Lwt.t) -> ('a * riak_key) Lwt.t
  val write_default' : riak_key -> Elem.t -> (Elem.t -> ('a * Elem.t) Lwt.t) -> 'a Lwt.t
end
