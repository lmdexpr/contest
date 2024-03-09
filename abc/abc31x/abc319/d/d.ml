open Core
open Scanf

let n, m = scanf "%d %Ld" Tuple2.create

let l = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let rec binsearch ~ok left right =
  let open Int64 in
  if abs (right - left) <= 1L then right
  else
    let mid = (right + left) / 2L in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let ok w =
  let rec loop i width line =
    if i >= n then Int64.(line <= m)
    else 
      let word = l.(i) and i = i + 1 in
      let open Int64 in
      let next = width + 1L + word in
      if next > w then
        word <= w &&
        loop i word (line + 1L)
      else
        loop i next line
  in
  Int64.(l.(0) <= w) &&
  loop 1 l.(0) 1L

let ans = binsearch ~ok 0L Int64.max_value

let () = printf "%Ld\n%!" ans
