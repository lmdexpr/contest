open Core
open Scanf

let n = scanf "%d" Fn.id

let xs = Array.create ~len:n (0, 0)
let ys = Array.create ~len:n (0, 0)
let () =
  for i = 0 to n - 1 do
    scanf " %d %d" @@ fun x y -> 
    xs.(i) <- x, i; 
    ys.(i) <- y, i;
  done

let () =
  let compare (x, _) (x', _) = compare x x' in
  Array.sort xs ~compare;
  Array.sort ys ~compare

type edge = {u : int; v : int; w : int;}

let es = 
  Iter.(0 -- (n - 2))
  |> Iter.fold
    (fun es i ->
      { w = fst xs.(i + 1) - fst xs.(i); u = snd xs.(i); v = snd xs.(i + 1) } ::
      { w = fst ys.(i + 1) - fst ys.(i); u = snd ys.(i); v = snd ys.(i + 1) } ::
      es
    )
    []
  |> List.to_array

let () = Array.sort es ~compare:(fun e e' -> compare e.w e'.w)

(* kruskal / クラスカル法 *)
let ans, _ =
  let dsu = Array.init (n+1) ~f:Union_find.create in
  Array.fold es ~init:(0, n) ~f:(fun (ans, size) {u; v; w;} ->
    if Union_find.same_class dsu.(u) dsu.(v) then (ans, size)
    else (
      Union_find.union dsu.(u) dsu.(v); (ans + w, size - 1)
    )
  )

let () = printf "%d\n%!" ans
