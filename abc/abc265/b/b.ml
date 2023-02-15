open Core
open Scanf

let n, m, t = scanf "%d %d %d" Tuple3.create

let a = Array.init (n - 1) ~f:(fun _ -> scanf " %d" ident)
let b = Array.init (n - 1) ~f:(const 0)
let () =
  for _ = 1 to m do
    let x, y = scanf " %d %d" Tuple2.create in
    let x = x - 2 in
    b.(x) <- b.(x) + y
  done

let yes = 
  Array.foldi a ~init:(true, t) ~f:(fun i (acc, t) a ->
      let t = t - a in
      acc && t > 0, t + b.(i)
    )
  |> Tuple2.get1

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
