(* Auto-generated from "yuki.atd" *)


type rlist = Yuki_t.rlist

type node = Yuki_t.node

type digit = Yuki_t.digit

type fg = Yuki_t.fg

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

val write_node :
  Bi_outbuf.t -> node -> unit
  (** Output a JSON value of type {!node}. *)

val string_of_node :
  ?len:int -> node -> string
  (** Serialize a value of type {!node}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_node :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> node
  (** Input JSON data of type {!node}. *)

val node_of_string :
  string -> node
  (** Deserialize JSON data of type {!node}. *)

val write_digit :
  Bi_outbuf.t -> digit -> unit
  (** Output a JSON value of type {!digit}. *)

val string_of_digit :
  ?len:int -> digit -> string
  (** Serialize a value of type {!digit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_digit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> digit
  (** Input JSON data of type {!digit}. *)

val digit_of_string :
  string -> digit
  (** Deserialize JSON data of type {!digit}. *)

val write_fg :
  Bi_outbuf.t -> fg -> unit
  (** Output a JSON value of type {!fg}. *)

val string_of_fg :
  ?len:int -> fg -> string
  (** Serialize a value of type {!fg}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_fg :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> fg
  (** Input JSON data of type {!fg}. *)

val fg_of_string :
  string -> fg
  (** Deserialize JSON data of type {!fg}. *)

