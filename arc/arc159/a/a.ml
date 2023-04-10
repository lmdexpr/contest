open Core
open Scanf

let n, _k = scanf "%d %d" Tuple2.create

let dist = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %d" @@ function 0 -> n + 1 | x -> x))
let () =
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        dist.(i).(j) <- min dist.(i).(j) (dist.(i).(k) + dist.(k).(j))
      done
    done
  done

let q = scanf " %d" ident

let solve s t =
  let s, t =
    let n = Int64.of_int n in
    Int64.(to_int_exn @@ (s - 1L) % n, to_int_exn @@ (t - 1L) % n)
  in
  if dist.(s).(t) > n then -1 else dist.(s).(t)

let () =
  for _ = 1 to q do
    scanf " %Ld %Ld" solve |> printf "%d\n%!"
  done
