open Core
open Scanf

module Modulo (M : sig include Int_intf.S val modulo : t end) = struct
  open M

  let rec inverse ?(b = modulo) ?(u = one) ?(v = zero) a =
    if b = zero then (u % modulo + modulo) % modulo
    else
      let t = a / b in
      let a, b = b, a - t * b in
      let u, v = v, u - t * v in
      inverse ~b ~u ~v a

  let ( + ) a b = (a % modulo + b % modulo) % modulo
  let ( * ) a b = (a % modulo * b % modulo) % modulo
  let ( / ) a b = a * inverse b
end
module M = Modulo (struct include Int let modulo = 998244353 end)

let n = scanf "%d" Fn.id
let a, b = scanf " %d %d" Tuple2.create
let p, q = scanf " %d %d" Tuple2.create

let dp = 
  Array.init (n + 1) ~f:(fun i ->
  Array.init (n + 1) ~f:(fun j ->
  Array.init 2 ~f:(fun _ ->
    if i = n then 1
    else if j = n then 0
    else 0
  )))

let () =
  for i = n - 1 downto 1 do
    for j = n - 1 downto 1 do
      let move x k = min (x + k) n in
      dp.(i).(j).(0) <- 
        Iter.(1 -- p) 
        |> Iter.map (fun k -> dp.(move i k).(j).(1))
        |> Iter.fold M.(+) dp.(i).(j).(0) 
        |> M.(fun x -> x / p);

      dp.(i).(j).(1) <-
        Iter.(1 -- q) 
        |> Iter.map (fun k -> dp.(i).(move j k).(0))
        |> Iter.fold M.(+) dp.(i).(j).(1) 
        |> M.(fun x -> x / q);
    done
  done

let ans = dp.(a).(b).(0)

let () = printf "%d\n%!" ans
