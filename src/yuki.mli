module RandomAccessList(Conn:Make.Conn)(Elem:Make.Elem) : sig
  val init : unit -> string Lwt.t
  val size : string -> int Lwt.t

  val cons : string -> ?key:string -> Elem.t -> string Lwt.t
  val head : string -> Elem.t Lwt.t
  val pop : string -> (Elem.t * string) Lwt.t
  (* head and tail raise Empty if list is empty *)

  val lookup : string -> int -> Elem.t Lwt.t
  (* lookup raises Subscript if index is out of bounds *)

  val page : string -> int -> int -> (Elem.t list * bool) Lwt.t
  val take_while : string -> (Elem.t -> bool) -> Elem.t list Lwt.t

  val fold_left : string -> ('a -> Elem.t -> 'a Lwt.t) -> 'a -> 'a Lwt.t
  val fold_right : string -> (Elem.t -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

  val map : string -> (Elem.t -> 'a) -> 'a list Lwt.t
end

module FingerTree(Conn:Make.Conn)(Elem:Make.Elem) : sig
  (*val init : unit -> string Lwt.t*)

  val cons : string -> Elem.t -> string Lwt.t
end

module Imperative : sig
  module RandomAccessList(Conn:Make.Conn)(Elem:Make.Elem) : sig
    val size : string -> int Lwt.t

    val cons : string -> ?key:string -> Elem.t -> unit Lwt.t
    val head : string -> Elem.t Lwt.t
    val pop : string -> Elem.t Lwt.t

    val lookup : string -> int -> Elem.t Lwt.t
    (* lookup raises Subscript if index is out of bounds *)

    val page : string -> int -> int -> (Elem.t list * bool) Lwt.t
    val take_while : string -> (Elem.t -> bool) -> Elem.t list Lwt.t

    val fold_left : string -> ('a -> Elem.t -> 'a Lwt.t) -> 'a -> 'a Lwt.t
    val fold_right : string -> (Elem.t -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

    val map : string -> (Elem.t -> 'a) -> 'a list Lwt.t
  end
end
