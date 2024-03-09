open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let p = Array.init (n - 1) ~f:(fun _ -> scanf " %d" Fn.id)

let dp = Array.create ~len:(n+1) (-1)
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ fun x y ->
    dp.(x) <- max dp.(x) y
  done;
  Array.iteri p ~f:(fun i p ->
    let i = i + 2 in
    dp.(i) <- max dp.(i) (dp.(p) - 1)
  )

let ans = Array.count dp ~f:(fun dp -> dp >= 0)

let () = printf "%d\n%!" ans
