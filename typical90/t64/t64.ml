open Core

let n, q = Scanf.scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let d = Array.init (n-1) ~f:(fun i -> a.(i + 1) - a.(i))

let e =
  Iter.(0 -- (n - 2))
  |> Iter.map (fun i -> abs d.(i))
  |> Iter.sum

let () =
  Iter.(1 -- q)
  |> Iter.fold
    (fun e _ ->
       let l, r, v = Scanf.scanf " %d %d %d" @@ fun l r v -> l - 1, r - 1, v in
       let diff i v =
         d.(i) <- d.(i) + v;
         abs d.(i) - abs (d.(i) - v)
       in
       let dl = if l < 1     then 0 else diff (l - 1) v
       and dr = if r > n - 2 then 0 else diff r (-v) in
       let e = dl + e + dr in
       printf "%d\n%!" e;
       e
    )
    e
  |> ignore
