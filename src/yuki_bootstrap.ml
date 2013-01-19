open Lwt
open Riak
open Bin_prot
open Utils
open Std

exception Empty

module Make(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Bootstrap = Make_binable(struct
    module Binable = struct
      type t = (string * string list) option with bin_io
    end

    type t = (Elem.t * string list) option
    let to_binable = function Some (x, p) -> Some (Elem.to_string x, p) | _ -> None
    let of_binable = function Some (x, p) -> Some (Elem.of_string x, p) | _ -> None
  end)

  module BootstrappedElem = struct
      type t = (Elem.t * string list) option
      let compare x y = match x, y with
        | Some (x, _), Some (y, _) -> Elem.compare x y
        | _ -> raise Not_found
      let of_string x = Bootstrap.bin_read_t ~pos_ref:(ref 0) (Bigstring.of_string x)
      let to_string x = Bigstring.to_string (bin_dump Bootstrap.bin_writer_t x)
      let bucket = Elem.bucket
  end

  module PrimH = Yuki_heap.Make(Conn)(BootstrappedElem)
  open BootstrappedElem (* expose E and H constructors *)

  let empty = None
  let is_empty = function None -> true | _ -> false

  let merge h1 h2 = match h1, h2 with
    | None, h -> return h
    | h, None -> return h
    | Some (x, p1), Some (y, p2) ->
        if Elem.compare x y <= 0 then
          lwt p = PrimH.insert h2 p1 in
          return (Some (x, p))
        else
          lwt p = PrimH.insert h1 p2 in
          return (Some (y, p))

  let insert x h = merge (Some (x, PrimH.empty)) h

  let find_min = function
    | None -> raise Empty
    | Some (x, _) -> return x

  let delete_min = function
    | None -> raise Empty
    | Some (x, p) ->
        if PrimH.is_empty p then return (x, None)
        else match_lwt PrimH.delete_min p with
          | (Some (y, p1), p2) ->
              lwt p' = PrimH.merge p1 p2 in
              return (x, Some (y, p'))
          | _ -> assert false
end
