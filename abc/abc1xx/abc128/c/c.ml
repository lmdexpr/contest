open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let mask = Array.init m ~f:(const 0)

let () =
  for i = 0 to m - 1 do
    let k = scanf " %d" ident in
    for _ = 1 to k do
      let s = scanf " %d" ident in
      mask.(i) <- mask.(i) lor (1 lsl (s - 1))
    done
  done

let p = Array.init m ~f:(fun _ -> scanf " %d" ident)

let ans =
  Iter.(0 -- (1 lsl n - 1))
  |> Iter.filter (fun pattern ->
      Array.for_alli mask ~f:(fun i mask -> Int.popcount (mask land pattern) % 2 = p.(i))
    )
  |> Iter.length

let () = printf "%d\n%!" ans
