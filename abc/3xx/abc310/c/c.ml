open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id) |> String.Set.of_array

let is_palindrome s =
  let rec loop i j =
    if i >= j then true
    else
      Char.(s.[i] = s.[j]) && loop (i + 1) (j - 1)
  in
  loop 0 (String.length s - 1)
let one = Set.count s ~f:(fun s -> String.length s = 1 || is_palindrome s)

let s = Set.filter s ~f:(fun s -> String.length s <> 1 && not (is_palindrome s))

let r = String.Set.map ~f:String.rev s

let ans = one + Set.(length (diff s r)) + Set.(length (inter s r) / 2)

let () = printf "%d\n%!" ans

