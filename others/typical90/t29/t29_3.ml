open Core

let w, n = Scanf.scanf "%d %d" Tuple2.create

module LazySegmentTree = struct
  type t = { seg: int array; laz: int array; size: int }

  let make n : t =
    let rec size sz = if sz >= n then sz else size (sz * 2) in
    let size = size 1 in
    let seg = Array.init (size * 2) ~f:(const 0) and laz = Array.init (size * 2) ~f:(const 0) in
    { seg; laz; size }

  open struct
    let left n = n * 2 and right n = n * 2 + 1

    let push { seg; laz; size } k =
      if k < size then begin
        laz.(left k)  <- max laz.(k) laz.(left k);
        laz.(right k) <- max laz.(k) laz.(right k)
      end;
      seg.(k) <- max seg.(k) laz.(k);
      laz.(k) <- 0
  end

  let update tree l r x =
    let rec update a b k =
      push tree k;
      if b <= l || r <= a then ()
      else if l <= a && b <= r then (tree.laz.(k) <- x; push tree k)
      else 
        let m  = (a + b) lsr 1 in
        let () = update a m (left k)
        and () = update m b (right k) in
        tree.seg.(k) <- max tree.seg.(left k) tree.seg.(right k)
    in
    update 0 tree.size 1

  let max tree l r =
    let rec max a b k =
      push tree k;
      if b <= l || r <= a then 0
      else if l <= a && b <= r then tree.seg.(k)
      else
        let m = (a + b) lsr 1 in
        let l = max a m (left k) 
        and r = max m b (right k) in
        Int.max l r
    in
    max 0 tree.size 1
end

let tree = LazySegmentTree.make w
let () =
  for _ = 1 to n do
    let l, r = Scanf.scanf " %d %d" @@ fun l r -> l - 1, r in
    let higher = LazySegmentTree.max tree l r + 1 in
    LazySegmentTree.update tree l r higher;
    printf "%d\n" higher
  done
