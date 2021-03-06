open Riak
open Ag_util

module type Conn = sig
  val with_connection : (riak_connection -> 'a Lwt.t) -> 'a Lwt.t
end

module type Stringable = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module type Elem = sig
  include Stringable
  val bucket : string
end

module type Ord = sig
  include Elem
  val compare : t -> t -> int
end

module type Monoid = sig
  include Stringable
  val zero : t
  val combine : t -> t -> t
end

module type Measure = sig
  type t
  module Monoid : Monoid
  val measure : t -> Monoid.t
end

module type OrderedMeasure = sig
  include Measure
  val compare : Monoid.t -> Monoid.t -> int
end
