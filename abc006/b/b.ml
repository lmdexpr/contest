open Core

let n = Scanf.scanf "%d" ident

let tribonacci = Array.init (n + 3) ~f:(const @@ -1)
let () =
  tribonacci.(0) <- 0;
  tribonacci.(1) <- 0;
  tribonacci.(2) <- 1;
  for i = 3 to n - 1 do
    tribonacci.(i) <- (tribonacci.(i - 1) + tribonacci.(i - 2) + tribonacci.(i - 3)) % 10007
  done

let () = printf "%d\n%!" tribonacci.(n - 1)
