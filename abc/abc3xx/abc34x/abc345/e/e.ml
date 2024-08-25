open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let balls = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let dp =
  (Array.init (n + 1) ~f:(fun _ ->
  (Array.init (k + 1) ~f:(fun _ ->
  (Array.init (n + 1) ~f:(const Int.min_value))))))

let () = 
  dp.(0).(0).(0) <- 0;
  for i = 1 to n do
    let _c, _v = balls.(i - 1) in
    for j = 0 to k do
      for l = 0 to n do
        ()
      done
    done
  done

let ans = 
  Array.max_elt ~compare dp.(n).(k)
  |> Option.value_exn

let ans = max ans (-1)

let () = printf "%d\n%!" ans
