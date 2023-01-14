open Core

let h, w = Scanf.scanf "%d %d" Tuple2.create

let a =
  Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> Scanf.scanf " %c" ident))

let (%%) v m = (v + m) % m

let q = Scanf.scanf " %d" ident
let x, y, d =
  Iter.(1 -- q)
  |> Iter.map (fun _ ->
      Scanf.scanf " %d %d" @@ fun a b -> a - 1, b - 1
    )
  |> Iter.fold
    (fun (x, y, d) (a, b) -> (a - x) %% h, (b - y) %% w, d * - 1)
    (0, 0, 1)

let restoration d size pos1 =
  Array.init size ~f:(fun i -> (pos1 + i * d) %% size)

let x = restoration d h x
let y = restoration d w y

let ans = Array.make_matrix ~dimx:h ~dimy:w '.'
let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      ans.(x.(i)).(y.(j)) <- a.(i).(j)
    done
  done;
  Array.iter ans ~f:(fun ans ->
      Array.iter ans ~f:(printf "%c");
      printf "\n%!"
    )
