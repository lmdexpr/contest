open Core

let n = Scanf.scanf "%d" ident

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d %d" Tuple2.create)

let (let+) x k = Iter.flat_map k x
let (let*) x k = Iter.map k x

let expects =
  let+ i = Iter.(0 -- (n - 1)) in
  let* j = Iter.(i + 1 -- (n - 1)) in
  let li, ri = a.(i) and lj, rj = a.(j) in
  let eij =
    let+ k = Iter.(li -- ri) in
    let* l = Iter.(lj -- rj) in
    (k, l)
  in
  let count = Iter.filter (Tuple2.uncurry (>)) eij |> Iter.length in
  float count /. float ((1 + ri - li) * (1 + rj - lj))

let () = printf "%.12f\n%!" @@ Iter.sumf expects
