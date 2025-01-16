open Core
open Scanf

let n = scanf " %d" Fn.id
let a = List.init n ~f:(fun _ -> scanf " %d" ((=) 1))

let a =
  let rec compress k acc = function
    | [ true; false; ] | [ false; true; ]  -> (k + 2) :: acc
    | [ true; true; ]  | [ false; false; ] -> 1 :: (k + 1) :: acc
    | hd1 :: (hd2 :: _ as rest) -> 
      let k = k + 1 in
      let k, acc = if Bool.(hd1 <> hd2) then k, acc else 0, k :: acc in
      compress k acc rest
    | _ -> acc
  in
  compress 0 [] a |> List.to_array

let n = Array.length a

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.append [| init |] @@ Array.folding_map a ~init ~f:(paired f)
let cumsum = cumsum ~init:0 ~f:(+) a

let interval k =
  Iter.(0 -- (n - k))
  |> Iter.map (fun i -> cumsum.(i + k) - cumsum.(i))
  |> Iter.max ~lt:(<)

let ans =
  interval 3 |> Option.value ~default:(
  interval 2 |> Option.value ~default:(
  interval 1 |> Option.value_exn))

let () = 
  printf "%d\n" ans
