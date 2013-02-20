open Lwt
open Ag_util
open Yuki_j
open Yojson.Safe

module RandomAccessList(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) = struct
  module Impl = Yuki_rlist.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(struct
    type t = rlist
    let of_string x = rlist_of_string x
    let to_string x = string_of_rlist x
    let bucket = Elem.bucket
  end)

  let init () = Client.put Impl.empty []

  let size head = Client.read head
    (Lwt_list.fold_left_s (fun a (w, _) -> return (a + w)) 0)

  let cons head ?key x = Client.write head (Impl.cons ?key x)
  let head head = Client.read head Impl.head
  let pop head = Client.write' head Impl.pop

  let lookup head i = Client.read head (Impl.lookup i)
  let page head i n = Client.read head (Impl.page i n)
  let skip_take_while head sp tp = Client.read head (Impl.skip_take_while sp tp)
  let take_while head p = Client.read head (Impl.take_while p)

  let fold_left head f x = Client.read head (Impl.fold_left f x)
  let fold_right head f x = Client.read head (Impl.fold_right f x)

  let map head f = Client.read head (Impl.map f)
end

module Queue(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) = struct
  module Impl = Yuki_queue.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(struct
    type t = Elem.t queue
    let of_string x = Json.from_string (read_queue Impl.reader) x
    let to_string x = Json.to_string (write_queue Impl.writer) x
    let bucket = Elem.bucket
  end)

  let init () = Client.put Impl.empty []

  let snoc head x = Client.write head (Impl.snoc x)
  let head head = Client.read head Impl.head
  let pop head = Client.write' head Impl.pop
end

module FingerTree(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem)(Measure:Yuki_make.Measure with type t = Elem.t) = struct
  module Impl = Yuki_tree.Make(Conn)(Elem)(Measure)
  module Client = Client.Make(Conn)(struct
    type t = string Yuki_tree_j.fg
    let of_string x = Json.from_string (Yuki_tree_j.read_fg Yojson.Safe.read_string) x
    let to_string x = Json.to_string (Yuki_tree_j.write_fg Yojson.Safe.write_string) x
    let bucket = Elem.bucket
  end)

  let init () = Client.put Impl.empty []

  let cons head x = Client.write head (Impl.cons x)
  let snoc head x = Client.write head (Impl.snoc x)
  let head head = Client.read head Impl.head
  let last head = Client.read head Impl.last

  let front head = Client.write' head Impl.front
  let rear head = Client.write' head Impl.rear

  let fold_left head f x = Client.read head (Impl.fold_left f x)
  let fold_right head f x = Client.read head (Impl.fold_right f x)
end

module Size(Elem:Yuki_make.Elem) = struct
  type t = Elem.t
  module Monoid = struct
    type t = int
    let of_string = int_of_string
    let to_string = string_of_int
    let zero = 0
    let combine = (+)
  end
  let measure _ = 1
end

module RandomAccessSequence(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) = struct
  include FingerTree(Conn)(Elem)(Size(Elem))

  let size head = Client.read head Impl.measure_t

  let delete head i = Client.write head (Impl.delete (compare (i + 1)))
  let insert head x i = Client.write head (Impl.insert x ((<=) i))
  let lookup head i = Client.read head (Impl.lookup ((<=) i))
end

module Product(M1:Yuki_make.Measure)(M2:Yuki_make.Measure with type t = M1.t) = struct
  type t = M1.t
  module Monoid = struct
    type t = M1.Monoid.t * M2.Monoid.t
    let of_string x = let (m1, m2) = pair_of_string read_string x in M1.Monoid.of_string m1, M2.Monoid.of_string m2
    let to_string (m1, m2) = string_of_pair write_string (M1.Monoid.to_string m1, M2.Monoid.to_string m2)
    let zero = M1.Monoid.zero, M2.Monoid.zero
    let combine (m1, m2) (m1', m2') = M1.Monoid.combine m1 m1', M2.Monoid.combine m2 m2'
  end
  let measure x = M1.measure x, M2.measure x
end

module OrderedSequence(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem)(Measure:Yuki_make.Measure with type t = Elem.t) = struct
  include FingerTree(Conn)(Elem)(Product(Measure)(Size(Elem)))

  let size head = Client.read head (fun x -> Impl.measure_t x >|= snd)

  let delete head i = Client.write head (Impl.delete (fun (m, _) -> compare i m))
  let insert head x = Client.write head (Impl.insert x (fun (m, _) -> Measure.measure x <= m))
  let lookup head i = Client.read head (Impl.lookup (fun (m, _) -> i <= m))
end

module Heap(Conn:Yuki_make.Conn)(Elem:Yuki_make.Ord) = struct
  module Impl = Yuki_bootstrap.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(Impl.BootstrappedElem)

  let init () = Client.put Impl.empty []

  let insert head x = Client.write head (Impl.insert x)

  let find_min head = Client.read head Impl.find_min
  let delete_min head = Client.write' head Impl.delete_min
end

module Imperative = struct
  module RandomAccessList(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) = struct
    module Impl = Yuki_rlist.Make(Conn)(Elem)
    module Client = Client.Make(Conn)(struct
      type t = rlist
      let of_string x = rlist_of_string x
      let to_string x = string_of_rlist x
      let bucket = Elem.bucket
    end)

    let size head = Client.read_default head Impl.empty
      (Lwt_list.fold_left_s (fun a (w, _) -> return (a + w)) 0)

    let cons head ?key x = Client.write_default head Impl.empty (Impl.cons ?key x)
    let head head = Client.read_default head Impl.empty Impl.head
    let pop head = Client.write_default' head Impl.empty Impl.pop

    let lookup head i = Client.read_default head Impl.empty (Impl.lookup i)
    let page head i n = Client.read_default head Impl.empty (Impl.page i n)
    let skip_take_while head sp tp = Client.read_default head Impl.empty (Impl.skip_take_while sp tp)
    let take_while head p = Client.read_default head Impl.empty (Impl.take_while p)

    let fold_left head f x = Client.read_default head Impl.empty (Impl.fold_left f x)
    let fold_right head f x = Client.read_default head Impl.empty (Impl.fold_right f x)

    let map head f = Client.read_default head Impl.empty (Impl.map f)
  end

  module Heap(Conn:Yuki_make.Conn)(Elem:Yuki_make.Ord) = struct
    module Impl = Yuki_bootstrap.Make(Conn)(Elem)
    module Client = Client.Make(Conn)(Impl.BootstrappedElem)

    let insert head x = Client.write_default head Impl.empty (Impl.insert x)

    let find_min head = Client.read_default head Impl.empty Impl.find_min
    let delete_min head = Client.write_default' head Impl.empty Impl.delete_min
  end
end
