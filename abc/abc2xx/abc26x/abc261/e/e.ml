open Core
open Scanf

let bit x k = (x lsr k) land 1

let bit_f a0 a1 = function
  | 0 -> a0
  | _ -> a1

let n, c = scanf "%d %d" Tuple2.create
let op   = Array.init n ~f:(fun _ -> scanf " %d %d" @@ fun t a ->
  let bit_f_01 op x = bit_f (op 0 x) (op 1 x) in
  fun k ->
  match t with
  | 1 -> bit_f_01 (land) (bit a k)
  | 2 -> bit_f_01 (lor)  (bit a k)
  | _ -> bit_f_01 (lxor) (bit a k)
)

let ans = Array.create ~len:n 0
let () =
  for k = 0 to 29 do
    let acc = ref (bit_f 0 1) in
    let crr = ref (bit c k) in
    for i = 0 to n - 1 do
      let op = op.(i) k in
      acc := bit_f (op @@ !acc 0) (op @@ !acc 1);
      crr := !acc !crr;
      ans.(i) <- ans.(i) lor (!crr lsl k)
    done
  done

let () = 
  Array.iter ~f:(printf "%d\n%!") ans
