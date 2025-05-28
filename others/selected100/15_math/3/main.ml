open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" @@ fun a -> a / 2)

let () =
  let num_power a =
    let rec aux a acc =
      if a mod 2 <> 0 then acc
      else
        aux (a / 2) (acc + 1)
    in
    aux a 0
  in
  if Array.map a ~f:num_power |> Int.Set.of_array |> Set.length <> 1 then (
    print_endline "0";
    exit 0;
  )

let rec gcd a =
  function 
  | 0 -> a
  | b -> gcd b (a % b)

let lcm a b = a / gcd a b * b

let t = Array.fold a ~init:a.(0) ~f:lcm

let ans = (m / t + 1) / 2

let () = printf "%d\n%!" ans
