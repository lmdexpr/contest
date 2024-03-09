open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let scan_1 _ = scanf " %d" ident
let scan_2 _ = scanf " %d %d" Tuple2.create

let rs, cs = scan_2 ()

let n = scan_1 ()
let add high x = 
  function
  | None   -> [ 0; x; high ]
  | Some v -> x :: v
let wall_w, wall_h =
  Iter.(1 -- n)
  |> Iter.fold
    (fun (wall_w, wall_h) _ ->
       let r, c = scan_2 () in
       Int.Map.update wall_w r ~f:(add (w + 1) c),
       Int.Map.update wall_h c ~f:(add (h + 1) r)
    )
    (Int.Map.empty, Int.Map.empty)

let f lst = let a = Array.of_list lst in Array.sort ~compare a; a
let wall_w = Int.Map.map wall_w ~f
let wall_h = Int.Map.map wall_h ~f

let lower_bound x arr =
  Option.value_exn (Array.binary_search arr ~compare `First_strictly_greater_than x)
let upper_bound x arr =
  Option.value_exn (Array.binary_search arr ~compare `Last_strictly_less_than x)

let border walls i ~default ~f = 
  Int.Map.find walls i
  |> Option.value_map ~default ~f:(fun arr -> arr.(f arr))

let solve r c l = function
  | 'L' -> r, max (c - l) @@ border wall_w r ~default:0 ~f:(upper_bound c) + 1
  | 'R' -> r, min (c + l) @@ border wall_w r ~default:(w+1) ~f:(lower_bound c) - 1
  | 'U' -> max (r - l) @@ border wall_h c ~default:0 ~f:(upper_bound r) + 1, c
  | 'D' -> min (r + l) @@ border wall_h c ~default:(h+1) ~f:(lower_bound r) - 1, c
  | _   -> invalid_arg ""

let solve (r, c) (d, l) =
  let r, c = solve r c l d in printf "%d %d\n%!" r c; r, c

let () =
  Iter.(1 -- scan_1 ())
  |> Iter.map (fun _ -> Scanf.scanf " %c %d" Tuple2.create)
  |> Iter.fold solve (rs, cs)
  |> ignore
