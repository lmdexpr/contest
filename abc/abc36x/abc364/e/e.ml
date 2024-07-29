open Core
open Scanf

let n, x, y = scanf "%d %d %d" Tuple3.create

let ab = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let dp =
  Array.init (n + 1) ~f:(fun _ ->
  Array.init (n + 1) ~f:(fun _ ->
  Array.init (x + 1) ~f:(fun _ -> 1_000_000_000)))

let (.!()<-) a i v = a.(i) <- min a.(i) v

let () = dp.(0).(0).(0) <- 0

let () =
  Array.iteri ab ~f:(fun i (a, b) ->
    for j = 0 to i do
      for k = 0 to x do
        dp.(i+1).(j).!(k) <- dp.(i).(j).(k);

        if k + a <= x then
          dp.(i+1).(j+1).!(k+a) <- dp.(i).(j).(k) + b
      done
    done
  )

let () =
  for j = n downto 0 do
    for k = 0 to x do
      if dp.(n).(j).(k) <= y then 
        let ans = min (j + 1) n in
        (printf "%d\n" ans; exit 0)
    done
  done
