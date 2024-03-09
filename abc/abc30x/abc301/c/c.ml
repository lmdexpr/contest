open Core
open Scanf

let s = scanf "%s" String.to_array
let t = scanf " %s" String.to_array

let c_to_i = function
  | '@' -> 0
  | c   -> Char.to_int c - Char.to_int 'a' + 1

let s_count = Array.init 27 ~f:(const 0)
let t_count = Array.init 27 ~f:(const 0)
let () =
  Array.iter s ~f:(fun c -> let i = c_to_i c in s_count.(i) <- s_count.(i) + 1);
  Array.iter t ~f:(fun c -> let i = c_to_i c in t_count.(i) <- t_count.(i) + 1);
  String.iter "atcoder" ~f:(fun c ->
      let i = c_to_i c in
      let m = max s_count.(i) t_count.(i) in
      if s_count.(0) < m - s_count.(i) || t_count.(0) < m - t_count.(i) then
        ( printf "No\n%!"; exit 0 )
      else begin
        s_count.(0) <- s_count.(0) - (m - s_count.(i)); s_count.(i) <- m;
        t_count.(0) <- t_count.(0) - (m - t_count.(i)); t_count.(i) <- m
      end
    )

let yes = Iter.(0 -- 25) |> Iter.for_all (fun i -> s_count.(i) = t_count.(i))

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
