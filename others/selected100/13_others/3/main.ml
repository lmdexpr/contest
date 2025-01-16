open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create

type circle =
  | Solid of (float * float) * float
  | Lack  of (float * float)

let c = Array.init (n + m) ~f:(fun i ->
  if i < n then
    scanf " %f %f %f" @@ fun x y r -> Solid ((x, y), r)
  else
    scanf " %f %f"    @@ fun x y   -> Lack (x, y)
)

let dist (x1, y1) (x2, y2) = 
  let x = x1 -. x2 and y = y1 -. y2 in
  Float.sqrt @@ x *. x +. y *. y

let ans =
  Iter.of_array_i c |> Iter.flat_map (fun (i, c1) ->
    Iter.(n -- (n + m - 1)) |> Iter.map (fun j ->
      if i = j then Float.infinity 
      else
        match c1, c.(j) with
        | Solid (p1, r), Lack p2 -> dist p1 p2 -. r
        | Lack   p1,     Lack p2 -> dist p1 p2 /. 2.
        | _                      -> assert false
    )
  )
  |> Iter.min ~lt:Float.(<)
  |> Option.value ~default:Float.infinity

let ans =
  Iter.(0 -- (n - 1)) |> Iter.map (fun i -> 
    match c.(i) with
    | Solid (_, r) -> r
    | Lack _       -> assert false
  ) |> Iter.fold Float.min ans

let () = printf "%.15f\n%!" ans
