(* unsolved *)

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
  let ( / ) a b = a * inverse b

  let ( + ) a b = (a % modulo + b % modulo) % modulo
  let ( * ) a b = (a % modulo * b % modulo) % modulo
end
module M = Modulo (struct include Int let modulo = 998244353 end)

let n = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ ->
  Array.init 6 ~f:(fun _ -> scanf " %d" Fn.id)
)

let numbers =
  Array.fold a ~init:Int.Set.empty ~f:(fun init -> Array.fold ~init ~f:Set.add)

let leq = Array.init n ~f:(fun _ -> Hashtbl.create (module Int))
let () =
  Set.iter numbers ~f:(fun x ->
    for i = 0 to n - 1 do
      let a = a.(i) in
      let data = Array.count a ~f:(fun y -> y <= x) in
      Hashtbl.set leq.(i) ~key:x ~data;
    done
  )

let maximum = Set.max_elt_exn numbers

let ans =
  Iter.(1 -- maximum)
  |> Iter.fold M.(fun acc x ->
    eprintf "x = %d\n%!" x;
    let p =
      Array.map leq ~f:(fun leq ->
        Hashtbl.find leq (x-1) |> Option.value ~default:0
      )
      |> Array.fold ~init:1 ~f:(fun acc x ->
        eprintf "1 - %d / 6\n%!" x;
        acc * x / 6
      )
    in
    acc + (1 - p)
    ) 0

let () = printf "%d\n%!" ans
