open Core
open Scanf

let n = scanf "%d" Fn.id
let a, b, c = scanf " %Ld %Ld %Ld" Tuple3.create

let d = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id))

let dijkstra k start =
  let inf  = Int64.shift_left 1L 60 in
  let dist = Array.create ~len:n inf in
  let used = Array.create ~len:n false in
  dist.(start) <- 0L;
  let rec dijkstra () =
    Iter.(0 -- (n - 1))
    |> Iter.filter (fun v -> not used.(v))
    |> Iter.min ~lt:Int64.(fun i j -> dist.(i) < dist.(j))
    |> function
    | None   -> dist
    | Some v ->
      used.(v) <- true;
      for u = 0 to n - 1 do
        dist.(u) <- Int64.min dist.(u) Int64.(dist.(v) + k d.(v).(u))
      done;
      dijkstra ()
  in
  dijkstra ()

let dist_car   = dijkstra Int64.(fun x -> x * a) 0
let dist_train = dijkstra Int64.(fun x -> x * b + c) (n - 1)
let ans =
  Iter.(0 -- (n - 1))
  |> Iter.map Int64.(fun i -> dist_car.(i) + dist_train.(i))
  |> Iter.min_exn ~lt:Int64.(<)

let () = printf "%Ld\n" ans
