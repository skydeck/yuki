(* Auto-generated from "yuki.atd" *)


type rlist = (int * string) list

type node = [
    `Node2 of (string * string * string)
  | `Node3 of (string * string * string * string)
]

type digit = [
    `One of (string * string)
  | `Two of (string * string * string)
  | `Three of (string * string * string * string)
  | `Four of (string * string * string * string * string)
]

type fg = [
    `Nil
  | `Single of string
  | `Deep of (string * digit * string * digit)
]
