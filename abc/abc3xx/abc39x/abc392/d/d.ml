open Core
open Scanf

let n = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> 
  let k   = scanf " %d" Fn.id in
  let tbl = Hashtbl.create (module Int) in
  for _ = 1 to k do
    let x = scanf " %d" Fn.id in
    Hashtbl.incr tbl x
  done;
  k, tbl
)

let ans = 
  Iter.(0 -- (n - 1))
  |> Iter.fold (fun acc i ->
    let k1, tbl1 = a.(i) in

    Iter.(i + 1 -- (n - 1))
    |> Iter.fold (fun acc j ->
      let k2, tbl2 = a.(j) in

      Float.max acc @@
      Hashtbl.fold tbl1 ~init:0. ~f:(fun ~key ~data:x acc ->
        match Hashtbl.find tbl2 key with
        | None   -> acc
        | Some y -> Float.add acc @@ float (x * y) /. float (k1 * k2)
      )
    ) acc
  ) 0.

let () = printf "%.15f\n%!" ans
