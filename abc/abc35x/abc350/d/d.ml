open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let uf = Array.init n ~f:Union_find.create

let () =
  for _ = 1 to m do
    scanf " %d %d" @@ fun v u ->
    Union_find.union uf.(v - 1) uf.(u - 1)
  done

let ans = Iter.(
  0 -- (n - 1)
  |> map (fun i -> Union_find.get uf.(i))
  |> group_by
  |> map List.length
  |> map (fun n -> n * (n - 1) / 2)
  |> sum
)
let ans = ans - m

let () = printf "%d\n%!" ans
