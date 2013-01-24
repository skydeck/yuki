open Lwt
open Riak
open Riak_kv_piqi
open Core
open Bin_prot
open Utils
open Type_class

module Make1(Conn:Make.Conn)(Elem:Make.Bin1) = struct
  type 'a t = {
    key : riak_key;
    value : 'a Elem.t;
    links : riak_key list;
  }

  let keys = List.map (function { Rpb_link.key = Some key } -> key | _ -> raise Not_found)
  let links = List.map (fun key -> { Rpb_link.bucket = Some Elem.bucket; key = Some key; tag = None })

  let get bin_reader key = Conn.with_connection (fun conn ->
    let reader = Elem.bin_reader_t bin_reader in
    match_lwt riak_get conn Elem.bucket key [] with
      | Some { obj_key = Some key; obj_value = Some value; obj_links = links } ->
          return { key; value = reader.read ~pos_ref:(ref 0) (Bigstring.of_string value); links = keys links }
      | _ -> raise Not_found
  )

  let put bin_writer ?key ?(ops=[Put_return_head true; Put_if_none_match true]) x ts = Conn.with_connection (fun conn ->
    match_lwt riak_put conn Elem.bucket key ~links:(links ts) (Bigstring.to_string (bin_dump (Elem.bin_writer_t bin_writer) x)) ops with
      | Some { obj_key = Some key } -> return key
      | _ -> (match key with
          | Some key -> return key
          | None -> raise Not_found
      )
  )
end

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  type t = {
    key : riak_key;
    value : Elem.t;
    links : riak_key list;
  }

  let keys = List.map (function { Rpb_link.key = Some key } -> key | _ -> raise Not_found)
  let links = List.map (fun key -> { Rpb_link.bucket = Some Elem.bucket; key = Some key; tag = None })

  let get key = Conn.with_connection (fun conn ->
    match_lwt riak_get conn Elem.bucket key [] with
      | Some { obj_key = Some key; obj_value = Some value; obj_links = links } ->
          return { key = key; value = Elem.of_string value; links = keys links }
      | _ -> raise Not_found
  )

  let put ?key ?(ops=[Put_return_head true; Put_if_none_match true]) x ts = Conn.with_connection (fun conn ->
    match_lwt riak_put conn Elem.bucket key ~links:(links ts) (Elem.to_string x) ops with
      | Some { obj_key = Some key } -> return key
      | _ -> (match key with
          | Some key -> return key
          | None -> raise Not_found
      )
  )

  let read key fn =
    lwt { value = x } = get key in
    fn x

  let read_default key empty fn =
    try_lwt
      read key fn
    with Not_found ->
      fn empty

  let write key fn =
    lwt { value = x } = get key in
    lwt x' = fn x in
    put x' []

  let write_default key empty fn =
    try_lwt
      lwt { value = x } = get key in
      lwt x' = fn x in
      lwt _ = put ~key ~ops:[Put_return_head true] x' [] in
      return ()
    with Not_found ->
      lwt x = fn empty in
      lwt _ = put ~key x [] in
      return ()

  let write' key fn =
    lwt { value = ts } = get key in
    lwt (x, ts') = fn ts in
    lwt key' = put ts' [] in
    return (x, key')

  let write_default' key empty fn =
    try_lwt
      lwt { value = ts } = get key in
      lwt (x, ts') = fn ts in
      lwt _ = put ~key ~ops:[Put_return_head true] ts' [] in
      return x
    with Not_found ->
      lwt (x, ts) = fn empty in
      lwt _ = put ~key ts [] in
      return x
end
