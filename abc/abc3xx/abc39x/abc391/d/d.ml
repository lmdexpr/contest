open Core
open Scanf

let n, w = scanf " %d %d" Tuple2.create

let blocks = Array.init (w + 1) ~f:(const [])
let () =
  for i = 0 to n - 1 do
    let x, y = scanf " %d %d" Tuple2.create in
    blocks.(x) <- (y, i) :: blocks.(x)
  done

let c = Array.create ~len:n 0
let d = Array.create ~len:(n + 1) (-1)

let () =
  Iter.(1 -- w) |> Iter.iter (fun i ->
    blocks.(i) 
    |> List.sort ~compare:(fun (x, _) (y, _) -> Int.ascending x y)
    |> List.iteri ~f:(fun j (y, a) ->
      c.(a) <- j;
      d.(j) <- max d.(j) y;
    );
    d.(List.length blocks.(i)) <- Int.pow 10 10;
  );
  for c = 1 to n do
    d.(c) <- max d.(c) @@ d.(c - 1) + 1
  done

let q = scanf " %d" Fn.id
let () =
  for _ = 1 to q do
    let t, a = scanf " %d %d" Tuple2.create in
    printf "%s\n%!" @@
    match d.(c.(a - 1)) with
    | exception _  -> "Yes"
    | d when t < d -> "Yes"
    | _            -> "No"
  done
