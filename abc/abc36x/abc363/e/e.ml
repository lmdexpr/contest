open Core
open Scanf

let h, w, y = scanf "%d %d %d" Tuple3.create

let a = 
  Array.init h ~f:(fun _ -> 
  Array.init w ~f:(fun _ -> scanf " %d" Fn.id))

let q = Array.init 100_001 ~f:(fun _ -> [])

let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      let x = a.(i).(j) in
      if i = 0 || i = h - 1 || j = 0 || j = w - 1 then
        q.(x) <- (i, j) :: q.(x)
    done
  done

let rec go ans level =
  match q.(level) with
  | []                               -> ans
  | (i, j) :: tl when a.(i).(j) <= 0 -> q.(level) <- tl; go ans level
  | (i, j) :: tl ->
    q.(level) <- tl; a.(i).(j) <- 0;
    [(0, 1); (0, -1); (1, 0); (-1, 0)]
    |> List.map    ~f:(fun (di, dj) -> i + di, j + dj)
    |> List.filter ~f:(fun (i, j) -> 0 <= i && i < h && 0 <= j && j < w)
    |> List.filter ~f:(fun (i, j) -> 0 < a.(i).(j))
    |> List.iter   ~f:(fun (i, j) ->
      let level = max level a.(i).(j) in
      q.(level) <- (i, j) :: q.(level);
    );
    go (ans - 1) level

let () =
  Iter.(1 -- y)
  |> Iter.scan go (h * w) |> Iter.drop 1
  |> Iter.iter (printf "%d\n")
