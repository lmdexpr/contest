open Core

module Segment_tree = struct
  type elt = int

  type t = { seg: elt array; size: int; unity: elt; op: elt -> elt -> elt }

  let make n unity op : t =
    let rec size sz = if sz >= n then sz else size (sz * 2) in
    let size = size 1 in
    let seg = Array.init (size * 2) ~f:(const unity) in
    { seg; size; unity; op }

  open struct
    let left n = n * 2 and right n = n * 2 + 1
  end

  let query { seg; size; unity; op } l r =
    let rec query l r i j =
      if i >= j then l, r
      else
        let i, l = if i land 1 = 0 then i, l else i + 1, op l seg.(i + 1)
        and j, r = if j land 1 = 0 then j, r else j + 1, op seg.(j - 1) r
        in
        query l r (i lsl 1) (j lsl 1)
    in
    Tuple2.uncurry op @@ query unity unity (l + size) (r + size)

  let update { seg; size; unity = _; op } k x =
    let k = k + size in
    seg.(k) <- x;
    Iter.iterate (fun k -> k lsl 1) (k lsl 1)
    |> Iter.filter (fun k -> k <> 0)
    |> Iter.iter (fun k -> seg.(k) <- op seg.(left k) seg.(right k))
end

module Fenwick_tree = struct
  type elt  = int
  let unity = 0

  type t = { bit: elt array array; size: int }

  let make n : t =
    let bit = Array.init 2 ~f:(fun _ -> Array.init (n + 1) ~f:(const unity)) in
    { bit; size = n }

  let rec add tree p k x =
    if k <= tree.size then begin
      tree.bit.(p).(k) <- tree.bit.(p).(k) + x;
      add tree p (k land -k) x
    end
  let add tree l r x = begin
    add tree 0 l @@ -x * (l - 1);
    add tree 0 r @@ x * (r - 1);
    add tree 1 l x;
    add tree 1 r @@ -x;
  end

  let rec sum ?(acc=unity) tree p k =
    if k <= 0 then acc
    else
      sum tree p ~acc:(acc + tree.bit.(p).(k)) @@ k - (k land -k)
  let sum tree k = sum tree 0 k + k * sum tree 1 k

  let query tree l r = sum tree (r - 1) - sum tree (l - 1)
end
