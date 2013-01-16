open Lwt
open Riak
open Yuki_j

exception Empty

module Make(Conn:Make.Conn)(Elem:Make.Elem)(Monoid:Make.Monoid) = struct
  type ('a, 'm) node =
    | Node2 of 'm * 'a * 'a
    | Node3 of 'm * 'a * 'a * 'a
  type ('a, 'm) digit =
    | One of 'm * 'a
    | Two of 'm * 'a * 'a
    | Three of 'm * 'a * 'a * 'a
    | Four of 'm * 'a * 'a * 'a * 'a
  type ('a, 'm) fg =
    | Nil (* not called Empty as in the paper to avoid a name
           * clash with the exception Empty *)
    | Single of 'a
    | Deep of 'm * ('a, 'm) digit * (*(('a, 'm) node, 'm) fg*) string * ('a, 'm) digit

  module Node(NodeElem:Make.Elem) = struct
    type t = (NodeElem.t, Monoid.t) node
    let of_string x = match node_of_string x with
      | `Node2 (v, a, b) -> Node2 (Monoid.of_string v, NodeElem.of_string a, NodeElem.of_string b)
      | `Node3 (v, a, b, c) -> Node3 (Monoid.of_string v, NodeElem.of_string a, NodeElem.of_string b, NodeElem.of_string c)
    let to_string x = string_of_node (match x with
      | Node2 (v, a, b) -> `Node2 (Monoid.to_string v, NodeElem.to_string a, NodeElem.to_string b)
      | Node3 (v, a, b, c) -> `Node3 (Monoid.to_string v, NodeElem.to_string a, NodeElem.to_string b, NodeElem.to_string c))
    let bucket = NodeElem.bucket
  end

  module Fg(FgElem:Make.Elem) = struct
    type t = (FgElem.t, Monoid.t) fg
    let digit_of_string = function
      | `One (v, a) -> One (Monoid.of_string v, FgElem.of_string a)
      | `Two (v, a, b) -> Two (Monoid.of_string v, FgElem.of_string a, FgElem.of_string b)
      | `Three (v, a, b, c) -> Three (Monoid.of_string v, FgElem.of_string a, FgElem.of_string b, FgElem.of_string c)
      | `Four (v, a, b, c, d) -> Four (Monoid.of_string v, FgElem.of_string a, FgElem.of_string b, FgElem.of_string c, FgElem.of_string d)
    let digit_to_string = function
      | One (v, a) -> `One (Monoid.to_string v, FgElem.to_string a)
      | Two (v, a, b) -> `Two (Monoid.to_string v, FgElem.to_string a, FgElem.to_string b)
      | Three (v, a, b, c) -> `Three (Monoid.to_string v, FgElem.to_string a, FgElem.to_string b, FgElem.to_string c)
      | Four (v, a, b, c, d) -> `Four (Monoid.to_string v, FgElem.to_string a, FgElem.to_string b, FgElem.to_string c, FgElem.to_string d)
    let of_string x = match fg_of_string x with
      | `Nil -> Nil
      | `Single a -> Single (FgElem.of_string a)
      | `Deep (v, pr, m, sf) -> Deep (Monoid.of_string v, digit_of_string pr, m, digit_of_string sf)
    let to_string x = string_of_fg (match x with
      | Nil -> `Nil
      | Single a -> `Single (FgElem.to_string a)
      | Deep (v, pr, m, sf) -> `Deep (Monoid.to_string v, digit_to_string pr, m, digit_to_string sf))
    let bucket = FgElem.bucket
  end

  module type Tree = sig
    module ElemT : Make.Elem
    module FgT : module type of Fg(ElemT)

    module Client : module type of Client.Make(Conn)(FgT)

    val empty : FgT.t
    val singleton : ElemT.t -> FgT.t
    val cons : measure:(ElemT.t -> Monoid.t) -> FgT.t -> ElemT.t -> FgT.t Lwt.t
    val snoc : measure:(ElemT.t -> Monoid.t) -> FgT.t -> ElemT.t -> FgT.t Lwt.t
    (*val front : FgT.t -> (FgT.t * ElemT.t)
    val head : FgT.t -> ElemT.t
    val last : FgT.t -> ElemT.t
    val tail : FgT.t -> FgT.t
    val init : FgT.t -> FgT.t
    val rear : FgT.t -> (FgT.t * ElemT.t)
    val size : FgT.t -> int
    val is_empty : FgT.t -> bool*)

    (*val fold_left : ('acc -> ElemT.t -> 'acc) -> 'acc -> FgT.t -> 'acc
    val fold_right : ('acc -> ElemT.t -> 'acc) -> 'acc -> FgT.t -> 'acc
    val iter : (ElemT.t -> unit) -> FgT.t -> unit
    val iter_right : ('a -> unit) -> FgT.t -> unit
    val map : ((ElemT.t -> 'b) -> FgT.t -> ('b, 'm) fg, 'b, 'm) wrap
    val map_right : ((ElemT.t -> 'b) -> FgT.t -> ('b, 'm) fg, 'b, 'm) wrap
    val append : FgT.t -> FgT.t -> FgT.t
    val reverse : FgT.t -> FgT.t*)
  end

  module rec Bootstrap : functor (Element : Make.Elem) -> (Tree with module ElemT = Element) = functor (Element : Make.Elem) ->
  struct
    module ElemT = Element
    module FgT = Fg(ElemT)

    module Client = Client.Make(Conn)(FgT)
    open Client

    module rec BootstrappedElem : (Make.Elem with type t = (ElemT.t, Monoid.t) node) = Node(ElemT)
    and PrimT : (Tree with module ElemT = BootstrappedElem) = Bootstrap(BootstrappedElem)

    let empty = Nil
    let singleton a = Single a

    let is_empty = function
      | Nil -> true
      | Single _ | Deep _ -> false
(*
    (*---------------------------------*)
    (*              fold               *)
    (*---------------------------------*)
    let fold_right_node f acc = function
      | Node2 (_, a, b) -> f (f acc b) a
      | Node3 (_, a, b, c) -> f (f (f acc c) b) a
    let fold_left_node f acc = function
      | Node2 (_, a, b) -> f (f acc a) b
      | Node3 (_, a, b, c) -> f (f (f acc a) b) c

    let fold_right_digit f acc = function
      | One (_, a) -> f acc a
      | Two (_, a, b) -> f (f acc b) a
      | Three (_, a, b, c) -> f (f (f acc c) b) a
      | Four (_, a, b, c, d) -> f (f (f (f acc d) c) b) a
    let fold_left_digit f acc = function
      | One (_, a) -> f acc a
      | Two (_, a, b) -> f (f acc a) b
      | Three (_, a, b, c) -> f (f (f acc a) b) c
      | Four (_, a, b, c, d) -> f (f (f (f acc a) b) c) d

    let rec fold_right : 'acc 'a 'm. ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc = fun f acc -> function
      | Nil -> acc
      | Single x -> f acc x
      | Deep (_, pr, m, sf) ->
          let acc = fold_right_digit f acc sf in
          let acc = fold_right (fun acc elt -> fold_right_node f acc elt) acc m in
          let acc = fold_right_digit f acc pr in
          acc
    let rec fold_left : 'acc 'a 'm. ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc = fun f acc -> function
      | Nil -> acc
      | Single x -> f acc x
      | Deep (_, pr, m, sf) ->
          let acc = fold_left_digit f acc pr in
          let acc = fold_left (fun acc elt -> fold_left_node f acc elt) acc m in
          let acc = fold_left_digit f acc sf in
          acc
*)
    (*---------------------------------*)
    (*     measurement functions       *)
    (*---------------------------------*)
    let measure_node = function
      | Node2 (v, _, _)
      | Node3 (v, _, _, _) -> v

    let measure_digit = function
      | One (v, _)
      | Two (v, _, _)
      | Three (v, _, _, _)
      | Four (v, _, _, _, _) -> v

    let measure_t_node = function
      | Nil -> Monoid.zero
      | Single x -> measure_node x
      | Deep (v, _, _, _) -> v
    let measure_t ~measure = function
      | Nil -> Monoid.zero
      | Single x -> measure x
      | Deep (v, _, _, _) -> v

    (*---------------------------------*)
    (*  a bunch of smart constructors  *)
    (*---------------------------------*)
    let node2 ~measure a b =
      Node2 (Monoid.combine (measure a) (measure b), a, b)
    let node2_node a b =
      Node2 (Monoid.combine (measure_node a) (measure_node b), a, b)

    let node3 ~measure a b c =
      Node3 (Monoid.combine (measure a) (Monoid.combine (measure b) (measure c)), a, b, c)
    let node3_node a b c =
      Node3 (Monoid.combine (measure_node a) (Monoid.combine (measure_node b) (measure_node c)), a, b, c)

    let deep pr m sf =
      let open PrimT.Client in
      let v = measure_digit pr in
      let v = Monoid.combine v (measure_t_node m) in
      let v = Monoid.combine v (measure_digit sf) in
      lwt { key } = put m [] in
      return (Deep (v, pr, key, sf))

    let one_node a =
      One (measure_node a, a)
    let one ~measure a =
      One (measure a, a)

    let two_node a b =
      Two (Monoid.combine (measure_node a) (measure_node b), a, b)
    let two ~measure a b =
      Two (Monoid.combine (measure a) (measure b), a, b)

    let three_node a b c =
      Three (Monoid.combine (Monoid.combine (measure_node a) (measure_node b)) (measure_node c), a, b, c)
    let three ~measure a b c =
      Three (Monoid.combine (Monoid.combine (measure a) (measure b)) (measure c), a, b, c)

    let four_node a b c d =
      Four (Monoid.combine (Monoid.combine (measure_node a) (measure_node b)) (Monoid.combine (measure_node c) (measure_node d)), a, b, c, d)
    let four ~measure a b c d =
      Four (Monoid.combine (Monoid.combine (measure a) (measure b)) (Monoid.combine (measure c) (measure d)), a, b, c, d)

    (*---------------------------------*)
    (*          cons / snoc            *)
    (*---------------------------------*)
    let cons_digit_node d x =
      match d with
        | One (v, a) -> Two (Monoid.combine (measure_node x) v, x, a)
        | Two (v, a, b) -> Three (Monoid.combine (measure_node x) v, x, a, b)
        | Three (v, a, b, c) -> Four (Monoid.combine (measure_node x) v, x, a, b, c)
        | Four _ -> assert false

    let cons_digit ~measure d x =
      match d with
        | One (v, a) -> Two (Monoid.combine (measure x) v, x, a)
        | Two (v, a, b) -> Three (Monoid.combine (measure x) v, x, a, b)
        | Three (v, a, b, c) -> Four (Monoid.combine (measure x) v, x, a, b, c)
        | Four _ -> assert false

    let snoc_digit_node d x =
      match d with
        | One (v, a) -> Two (Monoid.combine v (measure_node x), a, x)
        | Two (v, a, b) -> Three (Monoid.combine v (measure_node x), a, b, x)
        | Three (v, a, b, c) -> Four (Monoid.combine v (measure_node x), a, b, c, x)
        | Four _ -> assert false

    let snoc_digit ~measure d x =
      match d with
        | One (v, a) -> Two (Monoid.combine v (measure x), a, x)
        | Two (v, a, b) -> Three (Monoid.combine v (measure x), a, b, x)
        | Three (v, a, b, c) -> Four (Monoid.combine v (measure x), a, b, c, x)
        | Four _ -> assert false

    let rec cons_aux t a =
      match t with
        | Nil ->
            return (Single a)
        | Single b ->
            deep (one_node a) Nil (one_node b)
        (*| Deep (_, Four (_, b, c, d, e), m, sf) ->
            deep (two_node a b) (cons_aux m (node3_node c d e)) sf
        | Deep (v, pr, m, sf) ->
            return (Deep (Monoid.combine (measure_node a) v, cons_digit_node pr a, m, sf))*)
        | _ -> assert false

    let cons ~measure t a =
      match t with
        | Nil ->
            return (Single a)
        | Single b ->
            deep (one ~measure a) Nil (one ~measure b)
        (*| Deep (_, Four (_, b, c, d, e), m, sf) ->
            deep (two ~measure a b) (cons_aux m (node3 ~measure c d e)) sf
        | Deep (v, pr, m, sf) ->
            Deep (Monoid.combine (measure a) v, cons_digit ~measure pr a, m, sf)*)
        | _ -> assert false

    let rec snoc_aux t a =
      match t with
        | Nil ->
            return (Single a)
        | Single b ->
            deep (one_node b) Nil (one_node a)
        (*| Deep (_, pr, m, Four (_, b, c, d, e)) ->
            deep pr (snoc_aux m (node3_node b c d)) (two_node e a)
        | Deep (v, pr, m, sf) ->
            Deep (Monoid.combine v (measure_node a), pr, m, snoc_digit_node sf a)*)
        | _ -> assert false

    let snoc ~measure t a =
      match t with
        | Nil ->
            return (Single a)
        | Single b ->
            deep (one ~measure b) Nil (one ~measure a)
        (*| Deep (_, pr, m, Four (_, b, c, d, e)) ->
            deep pr (snoc_aux m (node3 ~measure b c d)) (two ~measure e a)
        | Deep (v, pr, m, sf) ->
            Deep (Monoid.combine v (measure a), pr, m, snoc_digit ~measure sf a)*)
        | _ -> assert false
  end
(*
  (*---------------------------------*)
  (*     various conversions         *)
  (*---------------------------------*)
  let to_tree_digit_node d =
    match d with
    | One (_, a) -> Single a
    | Two (v, a, b) -> Deep (v, one_node a, Nil, one_node b)
    | Three (v, a, b, c) -> Deep (v, two_node a b, Nil, one_node c)
    | Four (v, a, b, c, d) -> Deep (v, three_node a b c, Nil, one_node d)
  let to_tree_digit ~measure d =
    match d with
    | One (_, a) -> Single a
    | Two (v, a, b) -> Deep (v, one ~measure a, Nil, one ~measure b)
    | Three (v, a, b, c) -> Deep (v, two ~measure a b, Nil, one ~measure c)
    | Four (v, a, b, c, d) -> Deep (v, three ~measure a b c, Nil, one ~measure d)
  let to_tree_list ~measure = function
    | [] -> Nil
    | [a] -> Single a
    | [a; b] -> deep (one ~measure a) Nil (one ~measure b)
    | [a; b; c] -> deep (two ~measure a b) Nil (one ~measure c)
    | [a; b; c; d] -> deep (three ~measure a b c) Nil (one ~measure d)
    | _ -> assert false

  let to_digit_node = function
    | Node2 (v, a, b) -> Two (v, a, b)
    | Node3 (v, a, b, c) -> Three (v, a, b, c)
  let to_digit_list ~measure = function
    | [a] -> one ~measure a
    | [a; b] -> two ~measure a b
    | [a; b; c] -> three ~measure a b c
    | [a; b; c; d] -> four ~measure a b c d
    | _ -> assert false
  let to_digit_list_node = function
    | [a] -> one_node a
    | [a; b] -> two_node a b
    | [a; b; c] -> three_node a b c
    | [a; b; c; d] -> four_node a b c d
    | _ -> assert false

  (*---------------------------------*)
  (*     front / rear / etc.         *)
  (*---------------------------------*)
  let head_digit = function
    | One (_, a)
    | Two (_, a, _)
    | Three (_, a, _, _)
    | Four (_, a, _, _, _) -> a
  let last_digit = function
    | One (_, a)
    | Two (_, _, a)
    | Three (_, _, _, a)
    | Four (_, _, _, _, a) -> a
  let tail_digit_node = function
    | One _ -> assert false
    | Two (_, _, a) -> one_node a
    | Three (_, _, a, b) -> two_node a b
    | Four (_, _, a, b, c) -> three_node a b c
  let tail_digit ~measure = function
    | One _ -> assert false
    | Two (_, _, a) -> one ~measure a
    | Three (_, _, a, b) -> two ~measure a b
    | Four (_, _, a, b, c) -> three ~measure a b c
  let init_digit_node = function
    | One _ -> assert false
    | Two (_, a, _) -> one_node a
    | Three (_, a, b, _) -> two_node a b
    | Four (_, a, b, c, _) -> three_node a b c
  let init_digit ~measure = function
    | One _ -> assert false
    | Two (_, a, _) -> one ~measure a
    | Three (_, a, b, _) -> two ~measure a b
    | Four (_, a, b, c, _) -> three ~measure a b c

  type ('a, 'rest) view =
    | Vnil
    | Vcons of 'a * 'rest

  let rec view_left_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, (('a, 'm) node, 'm) fg) view =
    fun -> function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match view_left_aux m with
        | Vnil -> to_tree_digit_node sf
        | Vcons (a, m') -> deep (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep (tail_digit_node pr) m sf in
      Vcons (head_digit pr, vcons)
  let view_left ~measure = function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match view_left_aux m with
        | Vnil -> to_tree_digit ~measure sf
        | Vcons (a, m') -> deep (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep (tail_digit ~measure pr) m sf in
      Vcons (head_digit pr, vcons)

  let rec view_right_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, (('a, 'm) node, 'm) fg) view =
    fun -> function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match view_right_aux m with
        | Vnil -> to_tree_digit_node pr
        | Vcons (a, m') -> deep pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep pr m (init_digit_node sf) in
      Vcons (last_digit sf, vcons)
  let view_right ~measure = function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match view_right_aux m with
        | Vnil -> to_tree_digit ~measure pr
        | Vcons (a, m') -> deep pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep pr m (init_digit ~measure sf) in
      Vcons (last_digit sf, vcons)

  let head = function
    | Nil -> raise Empty
    | Single a -> a
    | Deep (_, pr, _, _) -> head_digit pr

  let last = function
    | Nil -> raise Empty
    | Single a -> a
    | Deep (_, _, _, sf) -> last_digit sf

  let tail ~measure t =
    match view_left ~measure t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> tl

  let front ~measure t =
    match view_left ~measure t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> (tl, hd)

  let init ~measure t =
    match view_right ~measure t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> tl

  let rear ~measure t =
    match view_right ~measure t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> (tl, hd)

  (*---------------------------------*)
  (*            append               *)
  (*---------------------------------*)
  let nodes =
    let add_digit_to digit l =
      match digit with
      | One (_, a) -> a :: l
      | Two (_, a, b) -> a :: b :: l
      | Three (_, a, b, c) -> a :: b :: c :: l
      | Four (_, a, b, c, d) -> a :: b :: c :: d :: l in

    let rec nodes_aux ~measure ts sf2 = (* no idea if this should be tail rec *)
      match ts, sf2 with
      | [], One _ -> assert false
      | [], Two (_, a, b)
      | [a], One (_, b) -> [node2 ~measure a b]
      | [], Three (_, a, b, c)
      | [a], Two (_, b, c)
      | [a; b], One (_, c) -> [node3 ~measure a b c]
      | [], Four (_, a, b, c, d)
      | [a], Three (_, b, c, d)
      | [a; b], Two (_, c, d)
      | [a; b; c], One (_, d) -> [node2 ~measure a b; node2 ~measure c d]
      | a :: b :: c :: ts, _ -> node3 ~measure a b c :: nodes_aux ~measure ts sf2
      | [a], Four (_, b, c, d, e)
      | [a; b], Three (_, c, d, e) -> [node3 ~measure a b c; node2 ~measure d e]
      | [a; b], Four (_, c, d, e, f) -> [node3 ~measure a b c; node3 ~measure d e f] in

    fun ~measure sf1 ts sf2 ->
      let ts = add_digit_to sf1 ts in
      nodes_aux ~measure ts sf2

  let rec app3 : 'a 'm.
      monoid:'m monoid -> measure:('a -> 'm) -> ('a, 'm) fg -> 'a list -> ('a, 'm) fg -> ('a, 'm) fg =
    fun ~measure t1 elts t2 ->
    match t1, t2 with
    | Nil, _ ->
      List.fold_right (fun elt acc -> cons ~measure acc elt) elts t2
    | _, Nil ->
      List.fold_left (fun acc elt -> snoc ~measure acc elt) t1 elts
    | Single x1, _ ->
      cons ~measure (List.fold_right (fun elt acc -> cons ~measure acc elt) elts t2) x1
    | _, Single x2 ->
      snoc ~measure (List.fold_left (fun acc elt -> snoc ~measure acc elt) t1 elts) x2
    | Deep (_, pr1, m1, sf1), Deep (_, pr2, m2, sf2) ->
      deep pr1 (app3 ~measure:measure_node m1 (nodes ~measure sf1 elts pr2) m2) sf2

  let append ~measure t1 t2 = app3 ~measure t1 [] t2

  (*---------------------------------*)
  (*            reverse              *)
  (*---------------------------------*)
  (* unfortunately, when reversing, we need to rebuild every annotation
   * because the monoid does not have to be commutative *)

  let reverse_digit_node rev_a = function
    | One (_, a) -> one_node (rev_a a)
    | Two (_, a, b) -> two_node (rev_a b) (rev_a a)
    | Three (_, a, b, c) -> three_node (rev_a c) (rev_a b) (rev_a a)
    | Four (_, a, b, c, d) -> four_node (rev_a d) (rev_a c) (rev_a b) (rev_a a)
  let reverse_digit ~measure = function
    | One _ as d -> d
    | Two (_, a, b) -> two ~measure b a
    | Three (_, a, b, c) -> three ~measure c b a
    | Four (_, a, b, c, d) -> four ~measure d c b a
  let reverse_node_node rev_a = function
    | Node2 (_, a, b) -> node2_node (rev_a b) (rev_a a)
    | Node3 (_, a, b, c) -> node3_node (rev_a c) (rev_a b) (rev_a a)
  let reverse_node ~measure = function
    | Node2 (_, a, b) -> node2 ~measure b a
    | Node3 (_, a, b, c) -> node3 ~measure c b a

  let rec reverse_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node -> ('a, 'm) node) -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, 'm) fg =
    fun reverse_a -> function
    | Nil -> Nil
    | Single a -> Single (reverse_a a)
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit_node reverse_a pr in
      let rev_sf = reverse_digit_node reverse_a sf in
      let rev_m = reverse_aux (reverse_node_node (reverse_a)) m in
      deep rev_sf rev_m rev_pr
  let reverse ~measure = function
    | Nil
    | Single _ as t -> t
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit ~measure pr in
      let rev_sf = reverse_digit ~measure sf in
      let rev_m = reverse_aux (reverse_node ~measure) m in
      deep rev_sf rev_m rev_pr

  (*---------------------------------*)
  (*             split               *)
  (*---------------------------------*)
  type ('a, 'rest) split = Split of 'rest * 'a * 'rest
  let split_digit ~measure p i = function
    | One (_, a) -> Split ([], a, [])
    | Two (_, a, b) ->
      let i' = Monoid.combine i (measure a) in
      if p i' then Split ([], a, [b]) else
        Split ([a], b, [])
    | Three (_, a, b, c) ->
      let i' = Monoid.combine i (measure a) in
      if p i' then Split ([], a, [b; c]) else
        let i'' = Monoid.combine i' (measure b) in
        if p i'' then Split ([a], b, [c]) else
          Split ([a; b], c, [])
    | Four (_, a, b, c, d) ->
      let i' = Monoid.combine i (measure a) in
      if p i' then Split ([], a, [b; c; d]) else
        let i'' = Monoid.combine i' (measure b) in
        if p i'' then Split ([a], b, [c; d]) else
          let i''' = Monoid.combine i'' (measure c) in
          if p i''' then Split ([a; b], c, [d]) else
            Split ([a; b; c], d, [])

  let deep_left ~measure pr m sf =
    match pr with
    | [] -> (
      match view_left ~measure:measure_node m with
      | Vnil -> to_tree_digit ~measure sf
      | Vcons (a, m') -> deep (to_digit_node a) m' sf
    )
    | _ ->
      deep (to_digit_list ~measure pr) m sf
  let deep_right ~measure pr m sf =
    match sf with
    | [] -> (
      match view_right ~measure:measure_node m with
      | Vnil -> to_tree_digit ~measure pr
      | Vcons (a, m') -> deep pr m' (to_digit_node a)
    )
    | _ ->
      deep pr m (to_digit_list ~measure sf)

  let rec split_tree : 'a 'm.
      monoid:'m monoid -> measure:('a -> 'm) -> ('m -> bool) -> 'm -> ('a, 'm) fg -> ('a, ('a, 'm) fg) split =
    fun ~measure p i -> function
    | Nil -> raise Empty
    | Single x -> Split (Nil, x, Nil)
    | Deep (_, pr, m, sf) ->
      let vpr = Monoid.combine i (measure_digit pr) in
      if p vpr then
        let Split (l, x, r) = split_digit ~measure p i pr in
        Split (to_tree_list ~measure l, x, deep_left ~measure r m sf)
      else
        let vm = Monoid.combine vpr (measure_t_node m) in
        if p vm then
          let Split (ml, xs, mr) = split_tree ~measure:measure_node p vpr m in
          let Split (l, x, r) = split_digit ~measure p (Monoid.combine vpr (measure_t_node ml)) (to_digit_node xs) in
          Split (deep_right ~measure pr ml l, x, deep_left ~measure r mr sf)
        else
          let Split (l, x, r) = split_digit ~measure p vm sf in
          Split (deep_right ~measure pr m l, x, to_tree_list ~measure r)

  let split ~measure f t =
    match t with
    | Nil -> (Nil, Nil)
    | _ ->
      if f (measure_t ~measure t) then
        let Split (l, x, r) = split_tree ~measure f Monoid.zero t in
        (l, cons ~measure r x)
      else
        (t, Nil)

  (*---------------------------------*)
  (*            lookup               *)
  (*---------------------------------*)
  let lookup_digit ~measure p i = function
    | One (_, a) -> Monoid.zero, a
    | Two (_, a, b) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else m_a, b
    | Three (_, a, b, c) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else
        let m_b = measure b in
        let i'' = Monoid.combine i' m_b in
        if p i'' then m_a, b else Monoid.combine m_a m_b, c
    | Four (_, a, b, c, d) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else
        let m_b = measure b in
        let i'' = Monoid.combine i' m_b in
        if p i'' then m_a, b else
          let m_c = measure c in
          let i''' = Monoid.combine i'' m_c in
          if p i''' then Monoid.combine m_a m_b, c else Monoid.combine (Monoid.combine m_a m_b) m_c, d

  let lookup_node ~measure p i = function
    | Node2 (_, a, b) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else m_a, b
    | Node3 (_, a, b, c) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else
        let m_b = measure b in
        let i'' = Monoid.combine i' m_b in
        if p i'' then m_a, b else Monoid.combine m_a m_b, c

  let rec lookup_tree : 'a 'm. monoid:'m monoid -> measure:('a -> 'm) -> ('m -> bool) -> 'm -> ('a, 'm) fg -> 'm * 'a =
    fun ~measure p i -> function
    | Nil -> raise Empty
    | Single x -> Monoid.zero, x
    | Deep (_, pr, m, sf) ->
      let m_pr = measure_digit pr in
      let vpr = Monoid.combine i m_pr in
      if p vpr then lookup_digit ~measure p i pr else
        let m_m = measure_t_node m in
        let vm = Monoid.combine vpr m_m in
        if p vm then
          let v_left, node = lookup_tree ~measure:measure_node p vpr m in
          let v, x = lookup_node ~measure p (Monoid.combine vpr v_left) node in
          Monoid.combine (Monoid.combine m_pr v_left) v, x
        else
          let v, x = lookup_digit ~measure p vm sf in
          Monoid.combine (Monoid.combine m_pr m_m) v, x

  let lookup ~measure p t =
    snd (lookup_tree ~measure p Monoid.zero t)

  (*---------------------------------*)
  (*        classic traversals       *)
  (*---------------------------------*)
  let iter f t =
    fold_left (fun () elt -> f elt) () t
  let iter_right f t =
    fold_right (fun () elt -> f elt) () t
  let map ~measure f t = (* suboptimal when the measure does not depend on 'a *)
    fold_left (fun acc elt -> snoc ~measure acc (f elt)) empty t
  let map_right ~measure f t =
    fold_right (fun acc elt -> cons ~measure acc (f elt)) empty t
*)
end
