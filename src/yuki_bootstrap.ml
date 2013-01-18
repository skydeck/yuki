open Lwt
open Riak
open Bin_prot
open Utils
open Std

exception Empty

module Bootstrap = struct
  type t = E | H of (string * string list) with bin_io
  let of_string x = bin_read_t ~pos_ref:(ref 0) (Bigstring.of_string x)
  let to_string x = Bigstring.to_string (bin_dump bin_writer_t x)
end

module Make(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module BootstrappedElem = struct
      type t = E | H of (Elem.t * string list)
      let compare x y = match x, y with
        | H (x, _), H (y, _) -> Elem.compare x y
        | _ -> raise Not_found
      let of_string x = match Bootstrap.of_string x with
        | Bootstrap.H (x, p) -> H (Elem.of_string x, p)
        | Bootstrap.E -> E
      let to_string x = Bootstrap.to_string (match x with
        | H (x, p) -> Bootstrap.H (Elem.to_string x, p)
        | E -> Bootstrap.E)
      let bucket = Elem.bucket
  end

  module PrimH = Yuki_heap.Make(Conn)(BootstrappedElem)
  open BootstrappedElem (* expose E and H constructors *)

  let empty = E
  let is_empty = function E -> true | _ -> false

  let merge h1 h2 = match h1, h2 with
    | E, h -> return h
    | h, E -> return h
    | H (x, p1), H (y, p2) ->
        if Elem.compare x y <= 0 then
          lwt p = PrimH.insert h2 p1 in
          return (H (x, p))
        else
          lwt p = PrimH.insert h1 p2 in
          return (H (y, p))

  let insert x h = merge (H (x, PrimH.empty)) h

  let find_min = function
    | E -> raise Empty
    | H (x, _) -> return x

  let delete_min = function
    | E -> raise Empty
    | H (x, p) ->
        if PrimH.is_empty p then return (x, E)
        else match_lwt PrimH.delete_min p with
          | (H (y, p1), p2) ->
              lwt p' = PrimH.merge p1 p2 in
              return (x, H (y, p'))
          | _ -> assert false
end
