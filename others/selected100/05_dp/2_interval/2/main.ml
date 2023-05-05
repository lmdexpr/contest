(* https://atcoder.jp/contests/joi2015ho/tasks/joi2015ho_b *)
open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let dp = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) (-1)
let memorize l r f =
  if dp.(l).(r) = -1 then dp.(l).(r) <- f ();
  dp.(l).(r)

let incr i = (i + 1) % n
let decr i = (i + n - 1) % n

let rec solve l r = memorize l r @@ fun () ->
  if l = r then 0
  else
    let l, r =
      if a.(l) > a.(r) then incr l, r else l, decr r
    in
    solve2 l r

and solve2 l r = memorize l r @@ fun () ->
  if l = r then a.(l)
  else
    max
      (a.(l) + solve (incr l) r)
      (a.(r) + solve l (decr r))

let ans =
  Array.mapi a ~f:(fun i a -> a + solve (incr i) (decr i))
  |> Array.max_elt ~compare
  |> Option.value ~default:0

let () = printf "%d\n%!" ans
