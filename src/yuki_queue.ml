open Lwt
open Riak
open Bin_prot
open Utils
open Std

exception Empty

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  (*type 'a digit = Zero | One of 'a | Two of 'a * 'a with bin_io
  type 'a queue = Shallow of 'a digit | Deep of 'a digit * ('a * 'a) queue * 'a digit
  type t = string queue*)

  module BinElem = Make_binable(struct
    module Binable = struct
      type t = string with bin_io
    end

    type t = Elem.t

    let to_binable = Elem.to_string
    let of_binable = Elem.of_string
  end)

  type 'a digit = Zero | One of 'a | Two of 'a * 'a with bin_io
  type 'a queue = Shallow of 'a digit | Deep of 'a digit * ('a * 'a) queue * 'a digit
  type 'a pair = ('a * 'a) with bin_io

  module Bootstrap = Make_binable(struct
    type t = Elem.t queue

    module Binable = struct
      type 'a t = Shallow of 'a digit | Deep of 'a digit * string * 'a digit with bin_io
    end

    let rec to_string : 'a . 'a Type_class.writer -> 'a queue -> string = fun w -> function
      | Shallow x -> Bigstring.to_string (bin_dump (Binable.bin_writer_t w) (Binable.Shallow x))
      | Deep (x1, xs, x2) ->
          let xs' = to_string (bin_writer_pair w) xs in
          Bigstring.to_string (bin_dump (Binable.bin_writer_t w) (Binable.Deep (x1, xs', x2)))

    let to_binable = function
      | Shallow x -> Binable.Shallow x
      | Deep (x1, xs, x2) ->
          let xs' = to_string (bin_writer_pair BinElem.bin_writer_t) xs in
          Binable.Deep (x1, xs', x2)

    let rec of_string : 'a . 'a Type_class.reader -> string -> 'a queue = fun w x ->
      let reader = Binable.bin_reader_t w in
      match reader.Type_class.read ~pos_ref:(ref 0) (Bigstring.of_string x) with
        | Binable.Shallow x -> Shallow x
        | Binable.Deep (x1, xs, x2) ->
            let xs' = of_string (bin_reader_pair w) xs in
            Deep (x1, xs', x2)

    let of_binable = function
      | Binable.Shallow x -> Shallow x
      | Binable.Deep (x1, xs, x2) ->
          let xs' = of_string (bin_reader_pair BinElem.bin_reader_t) xs in
          Deep (x1, xs', x2)
  end)

  (*module DeepElem = struct
    type t = Elem.t * Elem.t
    let of_string x = let (x1, x2) = pair_of_string x in (Elem.of_string x1, Elem.of_string x2)
    let to_string (x1, x2) = string_of_pair (Elem.to_string x1, Elem.to_string x2)
    let bucket = Elem.bucket
  end

  module Deep = MakeQ(Conn)

  module ShallowElem = struct
    type digit = Zero | One of Elem.t | Two of Elem.t * Elem.t
    type t = Shallow of digit | Deep of digit * string * digit
    let digit_of_string = function
      | `Zero -> Zero
      | `One x -> One (Elem.of_string x)
      | `Two (x1, x2) -> Two (Elem.of_string x1, Elem.of_string x2)
    let string_of_digit = function
      | Zero -> `Zero
      | One x -> `One (Elem.to_string x)
      | Two (x1, x2) -> `Two (Elem.to_string x1, Elem.to_string x2)
    let of_string x = match queue_of_string x with
      | `Shallow x -> Shallow (digit_of_string x)
      | `Deep (x1, xs, x2) -> Deep (digit_of_string x1, xs, digit_of_string x2)
    let to_string x = string_of_queue (match x with
      | Shallow x -> `Shallow (string_of_digit x)
      | Deep (x1, xs, x2) -> `Deep (string_of_digit x1, xs, string_of_digit x2))
    let bucket = Elem.bucket
  end

  module Client = Client.Make(Conn)(ShallowElem)
  open ShallowElem (* expose Shallow and Deep constructors *)

  let empty = Shallow Zero
  let is_empty = function Shallow Zero -> true | _ -> false

  let shallow x = Client.put (Shallow x) []
  let deep (x1, xs, x2) = Client.put (Deep (x1, xs, x2)) []

  let rec snoc y = function
    | Shallow Zero -> shallow (One y)
    | Shallow (One x) ->
        assert false
        (*lwt key = Deep.Client.put Deep.empty [] in
        deep (Two (x,y), key, Zero)*)
    (*| Deep (f, m, Zero) -> deep (f, m, One y)
    | Deep (f, m, One x) ->
        lwt m' = Deep.Client.get m >>= Deep.snoc (x,y) in
        deep (f, m', Zero)*)
    | _ -> assert false

  let head = function
    | Shallow Zero -> raise Empty
    | Shallow (One x)
    | Deep (One x, _, _)
    | Deep (Two (x, _), _, _) -> return x
    | _ -> assert false*)
end
