open Core

let n, k = Scanf.scanf "%d %Ld" Tuple2.create

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" @@ fun x -> x - 1)

let visited = Array.init n ~f:(const @@ -1)
let time_line = Array.init n ~f:(const @@ -1)
let rec simulate ?(count=0) t =
  if visited.(t) <> -1 then count, visited.(t)
  else begin
    visited.(t) <- count;
    time_line.(count) <- t;
    simulate ~count:(count + 1) a.(t)
  end
let count, uncycle_length = simulate 0

let cycle_length = count - uncycle_length

let pos =
  let open Int64 in
  let uncycle_length = of_int uncycle_length
  and cycle_length   = of_int cycle_length in
  if k <= uncycle_length then k
  else
    uncycle_length + (k - uncycle_length) % cycle_length

let () = printf "%d\n%!" @@ time_line.(Int64.to_int_exn pos) + 1
