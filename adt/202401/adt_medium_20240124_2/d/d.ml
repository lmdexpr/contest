open Core
open Scanf

let n, s, m, l = scanf "%d %d %d %d" @@ fun n s m l -> n, s, m, l

let dp = Array.create ~len:(n + 31) Int.max_value
let () =
  dp.(0) <- 0;
  for i = 0 to n do
    if dp.(i) < Int.max_value then begin
      dp.(i + 6) <- Int.min dp.(i + 6) (dp.(i) + s);
      dp.(i + 8) <- Int.min dp.(i + 8) (dp.(i) + m);
      dp.(i + 12) <- Int.min dp.(i + 12) (dp.(i) + l);
    end
  done

let ans =
  Iter.(n -- (n + 30))
  |> Iter.map (fun i -> dp.(i))
  |> Iter.min_exn ~lt:(<)

let () = printf "%d\n%!" ans
