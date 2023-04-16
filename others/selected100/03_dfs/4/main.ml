(* https://atcoder.jp/contests/joi2009yo/tasks/joi2009yo_d *)
open Core
open Scanf

let m = scanf "%d" ident
let n = scanf " %d" ident

let iced = Array.init n ~f:(fun _ -> Array.init m ~f:(fun _ -> scanf " %d" ((=) 1)))

let dfs w h iced =
  let rec dfs ?(depth=1) x y = 
    iced.(x).(y) <- false;
    Iter.of_list [ (x+1, y); (x-1, y); (x, y+1); (x, y-1) ]
    |> Iter.filter (fun (x, y) -> 0 <= x && x < h && 0 <= y && y < w && iced.(x).(y))
    |> Iter.fold (fun acc (x, y) -> max acc (dfs ~depth:(depth+1) x y)) depth
    |> fun depth ->
    iced.(x).(y) <- true; depth
  in
  Iter.(0 -- (h - 1)) |> Iter.flat_map (fun i ->
      Iter.(0 -- (w - 1)) |> Iter.map (fun j -> i, j)
    )
  |> Fn.flip Iter.fold 0 (fun acc (i, j) ->
      if not iced.(i).(j) then acc
      else 
        max acc (dfs i j)
    )

let ans = dfs m n iced

let () = printf "%d\n%!" ans
