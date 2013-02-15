module RandomAccessList(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) : sig
  val init : unit -> string Lwt.t
  val size : string -> int Lwt.t

  val cons : string -> ?key:string -> Elem.t -> string Lwt.t
  val head : string -> Elem.t Lwt.t
  val pop : string -> (Elem.t * string) Lwt.t
  (* head and tail raise Empty if list is empty *)

  val lookup : string -> int -> Elem.t Lwt.t
  (* lookup raises Subscript if index is out of bounds *)

  val page : string -> int -> int -> (Elem.t list * bool) Lwt.t
  val skip_take_while : string -> (Elem.t -> bool) -> (Elem.t -> bool) -> Elem.t list Lwt.t
  val take_while : string -> (Elem.t -> bool) -> Elem.t list Lwt.t

  val fold_left : string -> ('a -> Elem.t -> 'a Lwt.t) -> 'a -> 'a Lwt.t
  val fold_right : string -> (Elem.t -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

  val map : string -> (Elem.t -> 'a) -> 'a list Lwt.t
end

module Queue(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) : sig
  val init : unit -> string Lwt.t

  val snoc : string -> Elem.t -> string Lwt.t
  val head : string -> Elem.t Lwt.t
  val pop : string -> (Elem.t * string) Lwt.t
end

module Tree(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) : sig
  val init : unit -> string Lwt.t

  val cons : string -> Elem.t -> string Lwt.t
  val snoc : string -> Elem.t -> string Lwt.t
  val head : string -> Elem.t Lwt.t
  val last : string -> Elem.t Lwt.t

  val front : string -> (Elem.t * string) Lwt.t
  val rear : string -> (Elem.t * string) Lwt.t

  val reverse : string -> string Lwt.t
  val delete : string -> (int -> bool) -> string Lwt.t
  val lookup : string -> (int -> bool) -> Elem.t Lwt.t

  val fold_left : string -> ('a -> Elem.t -> 'a Lwt.t) -> 'a -> 'a Lwt.t
  val fold_right : string -> (Elem.t -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t
end

module Heap(Conn:Yuki_make.Conn)(Elem:Yuki_make.Ord) : sig
  val init : unit -> string Lwt.t

  val insert : string -> Elem.t -> string Lwt.t

  val find_min : string -> Elem.t Lwt.t
  val delete_min : string -> (Elem.t * string) Lwt.t
  (* find_min and delete_min raise Empty if heap is empty *)
end

module Imperative : sig
  module RandomAccessList(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) : sig
    val size : string -> int Lwt.t

    val cons : string -> ?key:string -> Elem.t -> unit Lwt.t
    val head : string -> Elem.t Lwt.t
    val pop : string -> Elem.t Lwt.t

    val lookup : string -> int -> Elem.t Lwt.t
    (* lookup raises Subscript if index is out of bounds *)

    val page : string -> int -> int -> (Elem.t list * bool) Lwt.t
    val skip_take_while : string -> (Elem.t -> bool) -> (Elem.t -> bool) -> Elem.t list Lwt.t
    val take_while : string -> (Elem.t -> bool) -> Elem.t list Lwt.t

    val fold_left : string -> ('a -> Elem.t -> 'a Lwt.t) -> 'a -> 'a Lwt.t
    val fold_right : string -> (Elem.t -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

    val map : string -> (Elem.t -> 'a) -> 'a list Lwt.t
  end

  module Heap(Conn:Yuki_make.Conn)(Elem:Yuki_make.Ord) : sig
    val insert : string -> Elem.t -> unit Lwt.t

    val find_min : string -> Elem.t Lwt.t
    val delete_min : string -> Elem.t Lwt.t
  end
end
