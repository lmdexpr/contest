open Core
open Scanf

let n, _ = scanf "%d %d" Tuple2.create

let product = Array.init n ~f:(fun _ ->
  let p, c = scanf " %d %d" Tuple2.create in
  p, c, Array.init c ~f:(fun _ -> scanf " %d" Fn.id) |> Int.Set.of_array
)

let satisfy i j =
  i <> j &&
  let pi, ci, fi = product.(i) in
  let pj, cj, fj = product.(j) in
  pi >= pj &&
  Set.is_subset ~of_:fj fi &&
  (pi > pj || cj > ci)

let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if satisfy i j then begin
        printf "Yes\n%!";
        exit 0
      end
    done
  done;
  printf "No\n%!"
