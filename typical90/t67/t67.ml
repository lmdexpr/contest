open Core

let zero = Char.to_int '0'

let int64_of_char c = Int64.of_int Char.(to_int c - zero)

let long_of_base8 = List.fold ~init:0L ~f:Int64.(fun acc c -> acc * 8L + int64_of_char c)

let char_of_base9 n = Char.of_int_exn @@ Int64.(to_int_exn @@ n % 9L) + zero

let rec base9_of_long ?(acc=[]) =
  function
  | 0L -> acc
  | n  -> base9_of_long ~acc:(char_of_base9 n :: acc) Int64.(n / 9L)

let step = 
  let (<<) = Fn.compose in
  let f = function
    | '8' -> '5'
    | c   -> c
  in
  List.map ~f << base9_of_long << long_of_base8

let step n _ = step n

let n, k = Scanf.scanf "%s %d" Tuple2.create
let () =
  match Iter.(1 -- k) |> Iter.fold step (String.to_list n) with
  | [] -> printf "0"
  | ls -> List.iter ls ~f:(printf "%c");
  printf "\n%!"
