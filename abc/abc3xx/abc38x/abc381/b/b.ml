open Core
open Scanf

module Array = struct
  include Array
  let ch f a i = a.(i) <- f a.(i)
  let incr = ch succ
end

let s = scanf " %s" Fn.id

let n = String.length s

let yes =
  let a   = Array.create ~len:30 0 in
  let c x = Char.to_int x - Char.to_int 'a' in
  Iter.of_str s |> Iter.iter (fun x -> Array.incr a (c x));

  n % 2 = 0 &&
  Iter.(0 -- (n/2 - 1)) |> Iter.for_all Char.(fun i -> s.[2 * i] = s.[2 * i + 1]) &&
  Array.for_all a ~f:(fun c -> c = 2 || c = 0)

let ans = if yes then "Yes" else "No"
let () =
  printf "%s\n%!" ans

