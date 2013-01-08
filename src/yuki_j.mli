(* Auto-generated from "yuki.atd" *)


type rlist = Yuki_t.rlist

type color = Yuki_t.color

val write_rlist :
  Bi_outbuf.t -> rlist -> unit
  (** Output a JSON value of type {!rlist}. *)

val string_of_rlist :
  ?len:int -> rlist -> string
  (** Serialize a value of type {!rlist}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rlist :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rlist
  (** Input JSON data of type {!rlist}. *)

val rlist_of_string :
  string -> rlist
  (** Deserialize JSON data of type {!rlist}. *)

val write_color :
  Bi_outbuf.t -> color -> unit
  (** Output a JSON value of type {!color}. *)

val string_of_color :
  ?len:int -> color -> string
  (** Serialize a value of type {!color}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_color :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> color
  (** Input JSON data of type {!color}. *)

val color_of_string :
  string -> color
  (** Deserialize JSON data of type {!color}. *)

