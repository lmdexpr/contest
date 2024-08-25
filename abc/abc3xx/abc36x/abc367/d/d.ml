open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let cumsum a =
  let paired f a b = let r = f a b in r, r in
  Array.folding_map a ~init:0 ~f:(paired Int.bit_xor)

let cumsum = cumsum a

let count_multiples n m =
  let count = ref 0 in
  let remc  = Hashtbl.create (module Int) in
  for i = 0 to n do
    let rem = cumsum.(i) % m in

    Hashtbl.update remc rem ~f:(function
        | Some x -> count := !count + x; x + 1
        | None   -> 1
      )
  done;
  !count

let ans = count_multiples n m

let () = printf "%d\n%!" ans
