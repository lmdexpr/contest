open Core

let n = Scanf.scanf "%d" ident

let a = List.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let gcd = List.reduce_exn a ~f:gcd

let a = List.map a ~f:(fun e -> e / gcd)

let rec f ?(c=0) n =
  if n = 1 then c
  else if n % 2 = 0 then f ~c:(c+1) (n / 2)
  else if n % 3 = 0 then f ~c:(c+1) (n / 3)
  else
    -1

let a = List.map a ~f

let () =
  if List.exists a ~f:(fun e -> e = -1) then print_endline "-1"
  else
    printf "%d\n" @@ List.sum (module Int) a ~f:ident
