open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let uf = Array.init n ~f:Union_find.create
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ fun u v -> Union_find.union uf.(u - 1) uf.(v - 1)
  done

let ans = 
  Array.map uf ~f:Union_find.get
  |> Set.of_array (module Int)
  |> Set.length

let () = printf "%d\n%!" ans
