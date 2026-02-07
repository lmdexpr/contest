open Core
open Scanf

let n = scanf " %d" Fun.id
let a = Array.append [| 0 |] @@ Array.init n ~f:(fun _ -> scanf " %d" Fun.id)

let () = Array.sort a ~compare:Int.ascending

let rec go acc carry k =
  if k <= 0 then
    if carry <= 0 then acc
    else
      go (carry % 10 :: acc) (carry / 10) 0
  else
    to_dec acc carry (a.(n-k + 1) - a.(n-k)) k

and to_dec acc carry d k =
  if d <= 0 then go acc carry (k - 1)
  else
    let k' = k + carry in
    let acc   = k' % 10 :: acc in
    let carry = k' / 10 in
    to_dec acc carry (d - 1) k

let () =
  go [] 0 n
  |> List.iter ~f:(printf "%d");
  printf "\n%!"
