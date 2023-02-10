open Core
open Scanf

let n = scanf "%d" ident

let t_max = 100000

let x = Array.init (t_max + 1) ~f:(const 0)
let a = Array.init (t_max + 1) ~f:(const 0)
let () =
  for _ = 1 to n do
    scanf " %d %d %d" @@ fun t xi ai ->
    x.(t) <- xi;
    a.(t) <- ai
  done

let dp = Array.init 5 ~f:(const Int.min_value)
let () =
  dp.(0) <- 0;
  for t = 1 to t_max do
    let pred = Array.copy dp in
    for p = 0 to 4 do
      if 0 < p then
        dp.(p) <- max dp.(p) pred.(p - 1);
      if p < 4 then
        dp.(p) <- max dp.(p) pred.(p + 1)
    done;
    dp.(x.(t)) <- dp.(x.(t)) + a.(t)
  done

let ans = Array.max_elt dp ~compare |> Option.value ~default:0

let () = printf "%d\n%!" ans
