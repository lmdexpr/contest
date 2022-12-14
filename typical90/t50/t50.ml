open Core

let n, l = Scanf.scanf "%d %d" Tuple2.create

let m = 1000000007

let dp = Array.init (max n l + 1) ~f:(const 0)

let () =
  for i = 0 to l - 1 do
    dp.(i) <- 1
  done;
  for i = l to n do
    dp.(i) <- (dp.(i-1) + dp.(i - l)) % m
  done;
  printf "%d\n%!" dp.(n)
