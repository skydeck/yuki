open Lwt
open Riak
open Riak_kv_piqi
open Printf

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  type t = {
    key : riak_key;
    value : Elem.t;
    vclock : riak_vclock;
    links : riak_key list;
  }

  let keys = List.map (function { Rpb_link.key = Some key } -> key | _ -> raise Not_found)
  let links = List.map (fun key -> { Rpb_link.bucket = Some Elem.bucket; key = Some key; tag = None })

  let get_conn key conn =
    match_lwt riak_get conn Elem.bucket key [] with
      | Some { obj_key = Some key; obj_value = Some value; obj_vclock = Some vclock; obj_links = links } ->
          return { key = key; value = Elem.of_string value; vclock = vclock; links = keys links }
      | _ -> raise Not_found

  let get key = Conn.with_connection (get_conn key)

  let put_conn ?key ?v ?(ops=[Put_return_head true; Put_if_none_match true]) x ts conn =
    match_lwt riak_put conn Elem.bucket key ~links:(links ts) (Elem.to_string x) ops with
      | Some { obj_key = Some key; obj_vclock = Some vclock } ->
          return { key = key; value = x; vclock = vclock; links = ts }
      | Some { obj_vclock = Some vclock; obj_value = Some rv } ->
        (match key with
          | Some key ->
            Printf.printf "->>>>>>>>>>>>>>>>>>> return value is '%s'" rv;
            return { key = key; value = x; vclock = vclock; links = ts }
          | None -> raise Not_found)
      | Some { obj_vclock = Some vclock } ->
        (match key with
          | Some key ->
            return { key = key; value = x; vclock = vclock; links = ts }
          | None -> raise Not_found)
      | _ -> raise Not_found

  let put ?key ?v ?ops x ts =
    Conn.with_connection (put_conn ?key ?v ?ops x ts)

  let read key fn =
    lwt { value = x } = get key in
    fn x

  let read_default key empty fn =
    try_lwt
      read key fn
    with Not_found ->
      fn empty

  let write key fn =
    lwt { value = x; vclock = v } = get key in
    lwt x' = fn x in
    lwt { key = key' } = put ~v x' [] in
    return key'

  let write_default key empty fn =
    try_lwt
      Conn.with_connection (fun conn ->
        lwt { value = x; vclock = v } = get_conn key conn in
        Printf.printf "[curr_head: %s]\n" (Elem.to_string x);
        lwt x' = fn x in
        Printf.printf "[new_head: %s]\n" (Elem.to_string x');
        lwt { value = y; vclock = v' } = put_conn ~key ~v ~ops:[Put_return_body true] x' [] conn in
        Printf.printf "write_default done. result: %s, old_vclock: %s, new_vclock: %s\n" (Elem.to_string y) v v';
        return ()
      )
    with Not_found ->
      lwt x = fn empty in
      Printf.printf "[new_head: %s]\n" (Elem.to_string x);
      lwt _ = put ~key x [] in
      Printf.printf "write_default done\n";
      return ()

  let write' key fn =
    lwt { value = ts; vclock = v } = get key in
    lwt (x, ts') = fn ts in
    lwt { key = key' } = put ~v ts' [] in
    return (x, key')

  let write_default' key empty fn =
    try_lwt
      lwt { value = ts; vclock = v } = get key in
      lwt (x, ts') = fn ts in
      lwt _ = put ~v ~key ~ops:[Put_return_head true] ts' [] in
      return x
    with Not_found ->
      lwt (x, ts) = fn empty in
      lwt _ = put ~key ts [] in
      return x
end
