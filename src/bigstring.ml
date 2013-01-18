open Bigarray
open Bin_prot
open Common
open Std

type t = buf

let of_string str =
  let len = String.length str in
  let buf = create_buf len in
  blit_string_buf str buf ~len;
  buf

let to_string buf =
  let len = Array1.dim buf in
  let str = String.create len in
  blit_buf_string buf str ~len;
  str
