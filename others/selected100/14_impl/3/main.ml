open Printf
open Scanf

module Cake = struct
  type piece = {
    w : int;
    h : int;
  }

  let create w h = { w; h }

  let size { w; h } = w * h

  let cut { w; h } s =
    let half = w + h in
    let s = s mod half in
    let p1, p2 =
      if s <= w then create s h, create (w - s) h
      else 
        create w (s - w), create w (h - (s - w))
    in
    if compare (size p1) (size p2) < 0 then p1, p2
    else 
      p2, p1

  type t = piece array

  let init w d : t = [| create w d |]

  let cut cake (p, s) =
    let n = Array.length cake in
    let t = cake.(p - 1) in
    Array.blit cake p cake (p - 1) (n - p);

    let p1, p2 = cut t s in
    cake.(n - 1) <- p1;
    Array.append cake [| p2 |]
end

let solve _n w d ps =
  let cake =
  Array.fold_left Cake.cut Cake.(init w d) ps
  |> Array.map  Cake.size
  in
  Array.sort compare cake;
  Array.(to_list cake)
  |> List.map string_of_int
  |> String.concat " " 
  |> printf "%s\n"

let () =
  let in2 _ =
    scanf " %d %d" @@ fun a b -> a, b
  in
  while true do
    scanf " %d %d %d" @@ fun n w d ->
    if n = 0 && w = 0 && d = 0 then exit 0
    else
      solve n w d Array.(init n in2)
  done
