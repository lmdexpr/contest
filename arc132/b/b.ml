open Core

let n = Scanf.scanf "%d" ident

let p = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let one_pos = Array.findi_exn p ~f:(fun _ v -> v = 1) |> Tuple2.get1

let ans =
  match p.((one_pos + 1) % n) with
  | 2 -> min one_pos (1 + (n - one_pos) + 1)
  | _ -> min (one_pos + 1 + 1) (1 + (n - one_pos - 1))

let () = printf "%d\n%!" ans
