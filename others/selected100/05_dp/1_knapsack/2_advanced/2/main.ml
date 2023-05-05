(* https://atcoder.jp/contests/joi2012yo/tasks/joi2012yo_d *)
open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let schedule = Array.init n ~f:(const @@ -1)
let () =
  for _ = 1 to k do
    let a, b = scanf " %d %d" Tuple2.create in
    schedule.(a - 1) <- b - 1
  done

let modulo = 10000
let ( +% ) a b = (a + b) % modulo

let dp = Array.init (n+1) ~f:(fun _ ->
    Array.init 3 ~f:(fun _ -> Array.init 3 ~f:(const 0))
  )
let () =
  dp.(0).(0).(0) <- 1;
  for i = 0 to n - 1 do
    let c = schedule.(i) in
    let c = if c = -1 then [ 0; 1; 2 ] else [ c ] in
    for a = 0 to 2 do
      for b = 0 to 2 do
        List.iter c ~f:(fun c ->
            if i < 2 || a <> b || b <> c then
              dp.(i + 1).(b).(c) <- dp.(i + 1).(b).(c) +% dp.(i).(a).(b)
          )
      done
    done
  done


let ans = Array.fold dp.(n) ~init:0 ~f:(fun init -> Array.fold ~init ~f:(+%))

let () = printf "%d\n%!" ans
