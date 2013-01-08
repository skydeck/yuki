open Lwt
open Riak
open Riak_piqi
open Yuki_j
open Batteries

module Make(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Client = Client.Make(Conn)(Elem)
  open Client

  let of_color color = Map.singleton "color" (string_of_color color)
  let to_color usermeta = color_of_string (Map.find "color" usermeta)

  let node ?key color x ts = put ?key ~usermeta:(of_color color) x ts

  (* Singleton tree. *)
  let singleton ?key x = node ?key `B x []

  (* Checking if this element is a member of a tree? *)
  let rec member x { value = y; links = ts } =
    match Elem.compare x y with
      | 0 -> return true
      | n when n < 0 ->
          (match ts with
            | l :: _ -> get l >>= member x
            | _ -> return false)
      | _ ->
          (match ts with
            | [_; r] -> get r >>= member x
            | _ -> return false)

  let turnR t = { t with usermeta = of_color `R }
  let turnB t = { t with usermeta = of_color `B }

  (* Finding the minimum element. Worst-case: O(log N) *)
  let rec minimum { value = x; links = ts } =
    match ts with
      | l :: _ -> get l >>= minimum
      | _ -> return x

  (* Finding the maximum element. Worst-case: O(log N) *)
  let rec maximum { value = x; links = ts } =
    match ts with
      | [_; r] -> get r >>= maximum
      | _ -> return x

  (*let is_color color { usermeta } = to_color usermeta = color

  let isBlackLeftBlack { links = ts; usermeta } =
    match to_color usermeta with
      | `B -> (match ts with
          | l :: _ -> get l >|= is_color `B
          | _ -> return true)
      | _ -> return false

  let isBlackLeftRed { links = ts; usermeta } =
    match to_color usermeta with
      | `B -> (match ts with
          | l :: _ -> get l >|= is_color `R
          | _ -> return true)
      | _ -> return false*)

  let balanceL { key; value = x; links = ts; usermeta } =
    match to_color usermeta with
      | `B -> assert false
      | _ -> assert false

  (*balanceL B (Node R ll@(Node R _ _ _) lx lr) x r =
      Node R (turnB ll) lx (Node B lr x r)
  balanceL c l x r = Node c l x r

  let balanceR B l@(Node R _ _ _) x r@(Node R _ _ _) =
    Node R (turnB l) x (turnB r)
-- x is Black since Red eliminated by the case above
-- x is either Node or Leaf
balanceR c l x (Node R rl rx rr) = Node c (Node R l x rl) rx rr
balanceR c l x r = Node c l x r*)

  let rec insert' ?key kx = function
    | { value = x; links = ts; usermeta } as t ->
        match Elem.compare kx x with
          | 0 -> return t
          | n when n < 0 ->
              (match ts with
                | l :: ts' ->
                  lwt { key = l' } = get l >>= insert' ?key kx in
                  balanceL { t with links = l' :: ts' }
                | _ ->
                  lwt key' = node ?key `R kx [] in
                  return { key = key'; value = kx; links = []; usermeta = (of_color `R) })
          | _ -> assert false


(*kx Leaf = Node R Leaf kx Leaf
insert' kx t@(Node c l x r) = case compare kx x of
    LT -> balanceL c (insert' kx l) x r
    GT -> balanceR c l x (insert' kx r)
    EQ -> t*)

  (* Insertion. Worst-case: O(log N) *)
  let insert ?key kx t =
    lwt t' = insert' ?key kx t in
    return (turnB t')
(*
----------------------------------------------------------------

{-| Deleting the minimum element. Worst-case: O(log N)

>>> deleteMin (fromList [5,3,7]) == fromList [5,7]
True
>>> deleteMin empty == empty
True
-}

deleteMin :: RBTree a -> RBTree a
deleteMin Leaf = empty
deleteMin t = case deleteMin' (turnR t) of
    Leaf -> Leaf
    s    -> turnB s

{-
  This deleteMin' keeps an invariant: the target node is always red.

  If the left child of the minimum node is Leaf, the right child
  MUST be Leaf thanks to the invariants of LLRB.
-}

deleteMin' :: RBTree a -> RBTree a
deleteMin' (Node R Leaf _ Leaf) = Leaf -- deleting the minimum
deleteMin' t@(Node R l x r)
  -- Red
  | isRed l      = Node R (deleteMin' l) x r
  -- Black-Black
  | isBB && isBR = hardMin t
  | isBB         = balanceR B (deleteMin' (turnR l)) x (turnR r)
  -- Black-Red
  | otherwise    = Node R (Node B (deleteMin' ll) lx lr) x r -- ll is Red
  where
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
    Node B ll lx lr = l -- to skip Black
deleteMin' _ = error "deleteMin'"

{-
  The hardest case. See slide 61 of:
	http://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf
-}

hardMin :: RBTree a -> RBTree a
hardMin (Node R l x (Node B (Node R rll rlx rlr) rx rr))
    = Node R (Node B (deleteMin' (turnR l)) x rll)
               rlx
               (Node B rlr rx rr)
hardMin _ = error "hardMin"

----------------------------------------------------------------

{-| Deleting the maximum

>>> deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
True
>>> deleteMax empty == empty
True
-}

deleteMax :: RBTree a -> RBTree a
deleteMax Leaf = empty
deleteMax t = case deleteMax' (turnR t) of
    Leaf -> Leaf
    s    -> turnB s

{-
  This deleteMax' keeps an invariant: the target node is always red.

  If the right child of the minimum node is Leaf, the left child
  is:

  1) A Leaf -- we can delete it
  2) A red node -- we can rotateR it and have 1).
-}

deleteMax' :: RBTree a -> RBTree a
deleteMax' (Node R Leaf _ Leaf) = Leaf -- deleting the maximum
deleteMax' t@(Node R l x r)
  | isRed l      = rotateR t
  -- Black-Black
  | isBB && isBR = hardMax t
  | isBB         = balanceR B (turnR l) x (deleteMax' (turnR r))
  -- Black-Red
  | otherwise    = Node R l x (rotateR r)
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
deleteMax' _ = error "deleteMax'"

{-
  rotateR ensures that the maximum node is in the form of (Node R Leaf _ Leaf).
-}

rotateR :: RBTree a -> RBTree a
rotateR (Node c (Node R ll lx lr) x r) = balanceR c ll lx (deleteMax' (Node R lr x r))
rotateR _ = error "rorateR"

{-
  The hardest case. See slide 56 of:
	http://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf
-}

hardMax :: RBTree a -> RBTree a
hardMax (Node R (Node B ll@(Node R _ _ _ ) lx lr) x r)
    = Node R (turnB ll) lx (balanceR B lr x (deleteMax' (turnR r)))
hardMax _              = error "hardMax"

----------------------------------------------------------------

{-| Deleting this element from a tree. Worst-case: O(log N)

>>> delete 5 (fromList [5,3]) == singleton 3
True
>>> delete 7 (fromList [5,3]) == fromList [3,5]
True
>>> delete 5 empty                         == empty
True
-}

delete :: Ord a => a -> RBTree a -> RBTree a
delete _ Leaf = empty
delete kx t = case delete' kx (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

delete' :: Ord a => a -> RBTree a -> RBTree a
delete' _ Leaf = Leaf
delete' kx (Node c l x r) = case compare kx x of
    LT -> deleteLT kx c l x r
    GT -> deleteGT kx c l x r
    EQ -> deleteEQ kx c l x r

deleteLT :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteLT kx R l x r
  | isBB && isBR = Node R (Node B (delete' kx (turnR l)) x rll) rlx (Node B rlr rx rr)
  | isBB         = balanceR B (delete' kx (turnR l)) x (turnR r)
  where
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
    Node B (Node R rll rlx rlr) rx rr = r
deleteLT kx c l x r = Node c (delete' kx l) x r

deleteGT :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteGT kx c (Node R ll lx lr) x r = balanceR c ll lx (delete' kx (Node R lr x r))
deleteGT kx R l x r
  | isBB && isBR = Node R (turnB ll) lx (balanceR B lr x (delete' kx (turnR r)))
  | isBB         = balanceR B (turnR l) x (delete' kx (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Node B ll@(Node R _ _ _) lx lr = l
deleteGT kx R l x r = Node R l x (delete' kx r)
deleteGT _ _ _ _ _ = error "deleteGT"

deleteEQ :: Ord a => a -> Color -> RBTree a -> a -> RBTree a -> RBTree a
deleteEQ _ R Leaf _ Leaf = Leaf
deleteEQ kx c (Node R ll lx lr) x r = balanceR c ll lx (delete' kx (Node R lr x r))
deleteEQ _ R l _ r
  | isBB && isBR = balanceR R (turnB ll) lx (balanceR B lr m (deleteMin' (turnR r)))
  | isBB         = balanceR B (turnR l) m (deleteMin' (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Node B ll@(Node R _ _ _) lx lr = l
    m = minimum r
deleteEQ _ R l _ r@(Node B rl rx rr) = Node R l m (Node B (deleteMin' rl) rx rr) -- rl is Red
  where
    m = minimum r
deleteEQ _ _ _ _ _ = error "deleteEQ"*)
end
