open Core

let _, s = Scanf.scanf "%d %s" Tuple2.create

let m = 1000000007

let answer =
  let atcoder = String.to_array "atcoder" in
  let init = [| 1; 0; 0; 0; 0; 0; 0; 0 |] in
  let dp now c =
    let next = Array.copy now in
    Array.iteri atcoder ~f:(fun j t ->
        if Char.(c = t) then
          next.(j+1) <- (next.(j+1) + now.(j)) % m
      );
    next
  in
  String.fold s ~init ~f:dp |> Array.last

let () = printf "%d\n%!" answer
