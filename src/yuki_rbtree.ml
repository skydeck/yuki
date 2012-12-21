module Make(Conn:Make.Conn)(Elem:Make.Ord) = struct
  type color = R | B | BB
  type tree = Leaf of color | Node of color * tree * Elem.t * tree

  let empty () = Leaf B

  let is_empty tree = tree = (Leaf B)

  let rec member e = function
	    | Node (_,l,x,r) ->
	        (match Elem.compare e x with
            | 0 -> true
            | n when n < 0 -> member e l
            | _ -> member e r)
      | Leaf _ -> false

  let insBalL (c, l, x, r) =
    match (c, l) with
	    | (B, Node (R, Node (R, s, y, t), z, u)) -> Node (R, Node (B, s, y, t), z, Node (B, u, x, r))
      | (B, Node (R, s, z, Node (R, t, y, u))) -> Node (R, Node (B, s, z, t), y, Node (B, u, x, r))
      | _ -> Node (c, l, x, r)

  let insBalR (c, l, x, r) =
    match (c, r) with
	    | (B, Node (R, s, y, Node (R, t, z, u))) -> Node (R, Node (B, l, x, s), y, Node (B, t, z, u))
      | (B, Node (R, Node (R, s, z, t), y, u)) -> Node (R, Node (B, l, x, s), z, Node (B, t, y, u))
      | _ -> Node (c, l, x, r)

  let rec insert_tree e t =
    match t with
	    | Node (c, l, x, r) ->
	        (match Elem.compare e x with
            | 0 -> t
		        | n when n < 0 -> insBalL (c, insert_tree e l, x, r)
            | _ -> insBalR (c, l, x, insert_tree e r))
	    | Leaf _ -> Node (R, Leaf B, e, Leaf B)

  let insert e t =
    match insert_tree e t with
	    | Node (_, left, el, right) -> Node (B, left, el, right)
      | Leaf _ -> assert false

  let rec min = function
	  | Node (_, Leaf _, x, _) -> x
    | Node (_, l, _, _) -> min l
    | Leaf _ -> assert false

  let unBB = function
	  | Leaf BB -> Leaf B
    | Node (BB, l, x, r) -> Node (B, l, x, r)
    | _ -> assert false

  let addB = function
	  | Node (R, l, x, r) -> Node (B, l, x, r)
    | Node (B, l, x, r) -> Node (BB, l, x, r)
    | Leaf B -> Leaf BB
    | _ -> assert false

  let value = function
	  | Node (_, _, x, _) -> x
    | Leaf _ -> assert false

  let left = function
	  | Node (_, l, _, _) -> l
    | Leaf _ -> assert false

  let right = function
	  | Node (_, _, _, r) -> r
    | Leaf _ -> assert false

  let isBlack = function
	  | Leaf B -> true
    | Node (B, _, _, _) -> true
    | _ -> false

  let isRed = function
	  | Node (R, _, _, _) -> true
    | _ -> false

  let double = function
	  | Node (BB, _, _, _) -> true
    | Leaf BB -> true
    | _ -> false

  let rec balDelL = function
	  | (B, d, y, Node (R, l, z, r)) ->
	      if double d then
          Node (B, balDelL (R, d, y, l), z, r)
	      else Node (B, d, y, Node (R, l, z, r))
    | (c, d, y, Node (B, l, z, r)) ->
	      if double d then
	        if isBlack l && isBlack r then
	          addB (Node (c, unBB d, y, Node (R, l, z, r)))
	        else if isRed l && isBlack r then
	          balDelL (c, d, y, Node (B, left l, value l, Node (R, right l, z, r)))
	        else Node (c, Node (B, unBB d, y, l), z, addB r)
	      else Node (c, d, y, Node (B, l, z, r))
    | (c, l, x, r) -> Node (c, l, x, r)

  let rec balDelR = function
	  | (B, Node (R, l, z, r), y, d) ->
	      if double d then
	        Node (B, l, z, balDelR (R, r, y, d))
	      else Node (B, Node (R, l, z, r), y, d)
    | (c, Node (B, l, z, r), y, d) ->
	      if double d then
	        if isBlack l && isBlack r then
	          addB (Node (c, Node (R, l, z, r), y, unBB d))
	        else if isBlack l && isRed r then
	          balDelR (c, Node (B, Node (R, l, z, left r), value r, right r), y, d)
	        else Node (c, addB l, z, Node (B, r, y, unBB d))
	      else Node (c, Node (B, l, z, r), y, d)
    | (c, l, x, r) -> Node (c, l, x, r)

  let rec delete_tree e t =
    match t with
	    | Node (R, Leaf _, x, Leaf _) ->
	        if Elem.compare e x = 0 then Leaf B else t
	    | Node (B, Leaf _, x, Leaf _) ->
	        if Elem.compare e x = 0 then Leaf BB else t
	    | Node (_, Leaf _, x, Node (_, l, y, r)) ->
	        if Elem.compare e x = 0 then
	          Node (B, l, y, r)
	        else if Elem.compare e y = 0 then
	          Node (B, Leaf B, x, Leaf B)
	        else t
	    | Node (_, Node (_, l, y, r), x, Leaf _) ->
	        if Elem.compare e x = 0 then
	          Node (B, l, y, r)
	        else if Elem.compare e y = 0 then
	          Node (B, Leaf B, x, Leaf B)
	        else t
	    | Node (c, l, x, r) ->
	        (match Elem.compare e x with
            | 0 ->
		            let m = min r in
		            balDelR (c, l, m, delete_tree m r)
            | n when n < 0 -> balDelL (c, delete_tree e l, x, r)
            | _ -> balDelR (c, l, x, delete_tree e r))
	    | Leaf _ -> t

  let delete e t =
    match delete_tree e t with
	    | Node (_, l, x, r) -> Node (B, l, x, r)
      | Leaf _ -> Leaf B
end
