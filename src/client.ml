open Lwt
open Riak
open Riak_piqi
open Riak_kv_piqi
open Batteries

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  module Usermeta = struct
    type t = (string, string) Map.t
    let of_list l = List.fold_right (fun { Rpb_pair.key; value } -> Map.add key (Option.get value)) l Map.empty
    let to_list m = Map.foldi (fun key value -> List.cons { Rpb_pair.key; value = Some value }) m []
  end

  type t = {
    key : riak_key;
    value : Elem.t;
    links : riak_key list;
    usermeta : Usermeta.t;
  }

  let keys = List.map (function { Rpb_link.key = Some key } -> key | _ -> raise Not_found)
  let links = List.map (fun key -> { Rpb_link.bucket = Some Elem.bucket; key = Some key; tag = None })

  let get key = Conn.with_connection (fun conn ->
    match_lwt riak_get conn Elem.bucket key [] with
      | Some { obj_key = Some key; obj_value = Some value; obj_links = links; obj_usermeta = usermeta } ->
          return { key; value = Elem.of_string value; links = keys links; usermeta = (Usermeta.of_list usermeta) }
      | _ -> raise Not_found
  )

  let put ?key ?usermeta ?(ops=[Put_return_head true; Put_if_none_match true]) x ts = Conn.with_connection (fun conn ->
    match_lwt riak_put conn Elem.bucket key ?usermeta:(Option.map Usermeta.to_list usermeta) ~links:(links ts) (Elem.to_string x) ops with
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
