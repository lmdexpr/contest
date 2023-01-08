open Core

let n = Scanf.scanf "%d" ident

let x = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

(* 素数をハードコード *)
let primes = [| 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47 |]

let product_bit bits =
  Iter.(0 -- 14)
  |> Iter.filter (fun x -> bits land (1 lsl x) <> 0)
  |> Iter.map (Array.get primes)
  |> Iter.fold ( * ) 1

let rec gcd = function
  | 0, b -> b
  | a, 0 -> a
  | a, b -> gcd @@ if a < b then a, (b % a) else (a % b), b

(* bit 全探索 *)
let () =
  Iter.(0 -- (1 lsl 16 - 1))
  |> Iter.map product_bit
  |> Iter.filter (fun y -> Array.for_all x ~f:(fun x -> gcd (x, y) <> 1))
  |> Iter.min_exn ~lt:(<)
  |> printf "%d\n%!"
