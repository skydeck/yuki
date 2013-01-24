open Riak
open Core

module type Conn = sig
  val with_connection : (riak_connection -> 'a Lwt.t) -> 'a Lwt.t
end

module type Elem = sig
  include Stringable.S
  val bucket : string
end

module type Ord = sig
  include Elem
  val compare : t -> t -> int
end

module type Bin1 = sig
  include Binable.S1
  val bucket : string
end
