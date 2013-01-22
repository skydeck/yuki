open Lwt
open Riak
open Core
open Bin_prot
open Utils
open Std
open Common
open Read_ml
open Write_ml
open Size
open Type_class

exception Empty

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  module type Make_binable_spec' = sig
    module Binable : Binable.S1

    type 'a t

    val to_binable : 'a writer -> 'a t -> 'a Binable.t
    val of_binable : 'a reader -> 'a Binable.t -> 'a t
  end

  module Make_binable' (S : Make_binable_spec') = struct
    module B = S.Binable

    let bin_writer = cnv_writer Elem.to_string bin_writer_string

    let bin_reader = cnv_reader Elem.of_string bin_reader_string

    let bin_size_t t =
      B.bin_size_t bin_writer.size (S.to_binable bin_writer t)

    let bin_write_t buf ~pos t =
      B.bin_write_t bin_writer.unsafe_write buf ~pos (S.to_binable bin_writer t)

    let bin_write_t_ sptr eptr t =
      B.bin_write_t_ bin_writer.unsafe_write sptr eptr (S.to_binable bin_writer t)

    let bin_read_t buf ~pos_ref =
      S.of_binable bin_reader (B.bin_read_t bin_reader.unsafe_read buf ~pos_ref)

    let bin_read_t_ sptr_ptr eptr =
      S.of_binable bin_reader (B.bin_read_t_ bin_reader.unsafe_read sptr_ptr eptr)

    let bin_read_t__ sptr_ptr eptr n =
      S.of_binable bin_reader (B.bin_read_t__ bin_reader.unsafe_read sptr_ptr eptr n)

    let bin_writer_t =
      {
        size = bin_size_t;
        write = bin_write_t;
        unsafe_write = bin_write_t_;
      }

    let bin_reader_t =
      {
        read = bin_read_t;
        unsafe_read = bin_read_t_;
        unsafe_vtag_read = bin_read_t__;
      }

    let bin_t =
      {
        writer = bin_writer_t;
        reader = bin_reader_t;
      }
  end

  type 'a digit = Zero | One of 'a | Two of 'a * 'a with bin_io
  type 'a queue = Shallow of 'a digit | Deep of 'a digit * ('a * 'a) queue * 'a digit

  module Bootstrap = Make_binable'(struct
    type 'a t = 'a queue

    module Binable = struct
      type 'a t = Shallow of 'a digit | Deep of 'a digit * string * 'a digit with bin_io
    end

    let rec to_string : 'a . 'a writer -> 'a queue -> string = fun w -> function
      | Shallow x -> Bigstring.to_string (bin_dump (Binable.bin_writer_t w) (Binable.Shallow x))
      | Deep (x1, xs, x2) ->
          let xs' = to_string (bin_writer_pair w w) xs in
          Bigstring.to_string (bin_dump (Binable.bin_writer_t w) (Binable.Deep (x1, xs', x2)))

    let to_binable = fun w -> function
      | Shallow x -> Binable.Shallow x
      | Deep (x1, xs, x2) ->
          let xs' = to_string (bin_writer_pair w w) xs in
          Binable.Deep (x1, xs', x2)

    let rec of_string : 'a . 'a reader -> string -> 'a queue = fun w x ->
      let reader = Binable.bin_reader_t w in
      match reader.read ~pos_ref:(ref 0) (Bigstring.of_string x) with
        | Binable.Shallow x -> Shallow x
        | Binable.Deep (x1, xs, x2) ->
            let xs' = of_string (bin_reader_pair w w) xs in
            Deep (x1, xs', x2)

    let of_binable = fun w -> function
      | Binable.Shallow x -> Shallow x
      | Binable.Deep (x1, xs, x2) ->
          let xs' = of_string (bin_reader_pair w w) xs in
          Deep (x1, xs', x2)
  end)

  module Client = Client.Make(Conn)(struct
    type t = Elem.t queue
    let of_string x = Bootstrap.bin_read_t ~pos_ref:(ref 0) (Bigstring.of_string x)
    let to_string x = Bigstring.to_string (bin_dump Bootstrap.bin_writer_t x)
    let bucket = Elem.bucket
  end)

  let empty = Shallow Zero
  let is_empty = function Shallow Zero -> true | _ -> false

  let shallow x = return (Shallow x)
  let deep (x1, xs, x2) = return (Deep (x1, xs, x2))

  let rec snoc : 'a. 'a -> 'a queue -> 'a queue Lwt.t = fun y -> function
    | Shallow Zero -> shallow (One y)
    | Shallow (One x) -> deep (Two (x,y), empty, Zero)
    | Deep (f, m, Zero) -> deep (f, m, One y)
    | Deep (f, m, One x) ->
      lwt m' = snoc (x,y) m in
      deep (f, m', Zero)
    | _ -> assert false

  and head : 'a. 'a queue -> 'a Lwt.t = function
    | Shallow Zero -> raise Empty
    | Shallow (One x)
    | Deep (One x, _, _)
    | Deep (Two (x, _), _, _) -> return x
    | _ -> assert false

   and pop : 'a. 'a queue -> ('a * 'a queue) Lwt.t = function
     | Shallow Zero -> raise Empty
     | Shallow (One x) -> return (x, empty)
     | Deep (Two (x,y), m, r) ->
       lwt m' = deep (One y, m, r) in
       return (x, m')
     | Deep (One x, q, r) ->
         if is_empty q then
           lwt q' = shallow r in
           return (x, q')
         else
           lwt ((y, z), q') = pop q in
           lwt q' = deep (Two (y,z), q', r) in
           return (x, q')
     | _ -> assert false
end
