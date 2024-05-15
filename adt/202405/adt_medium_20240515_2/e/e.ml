open Core
open Scanf

let n, d = scanf "%d %d" Tuple2.create
let p = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let infected = Array.create ~len:n false

let dist2 (x, y) (x', y') = (x - x') * (x - x') + (y - y') * (y - y')

let rec solve = function
  | []                            -> ()
  | i :: xs when not infected.(i) -> solve xs
  | i :: xs -> 
    for j = 0 to n - 1 do
      if i <> j && not infected.(j) && dist2 p.(i) p.(j) <= d * d then (
        infected.(j) <- true;
        solve (j :: xs)
      )
    done

let () = 
  infected.(0) <- true;
  solve [0]

let () = 
  let f x = if x then printf "Yes\n" else printf "No\n" in
  Array.iter infected ~f
