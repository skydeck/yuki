open Riak
open Bin_prot
open Type_class

module Make1(Conn:Make.Conn)(Elem:Make.Bin1) : sig
  type 'a t = {
    key : riak_key;
    value : 'a Elem.t;
    links : riak_key list;
  }

  val get : 'a reader -> riak_key -> 'a t Lwt.t
  val put : 'a writer -> ?key:riak_key -> ?ops:riak_put_option list -> 'a Elem.t -> riak_key list -> riak_key Lwt.t
end

module Make(Conn:Make.Conn)(Elem:Make.Elem) : sig
  type t = {
    key : riak_key;
    value : Elem.t;
    links : riak_key list;
  }

  val get : riak_key -> t Lwt.t
  val put : ?key:riak_key -> ?ops:riak_put_option list -> Elem.t -> riak_key list -> riak_key Lwt.t

  val read : riak_key -> (Elem.t -> 'a Lwt.t) -> 'a Lwt.t
  val read_default : riak_key -> Elem.t -> (Elem.t -> 'a Lwt.t) -> 'a Lwt.t

  val write : riak_key -> (Elem.t -> Elem.t Lwt.t) -> riak_key Lwt.t
  val write_default : riak_key -> Elem.t -> (Elem.t -> Elem.t Lwt.t) -> unit Lwt.t
  val write' : riak_key -> (Elem.t -> ('a * Elem.t) Lwt.t) -> ('a * riak_key) Lwt.t
  val write_default' : riak_key -> Elem.t -> (Elem.t -> ('a * Elem.t) Lwt.t) -> 'a Lwt.t
end
