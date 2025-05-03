open Core
open Scanf

let n = scanf " %d" Fn.id

let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)
let t = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let rotate90 a =
  let b = Array.copy_matrix a in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      b.(j).(n - i - 1) <- a.(i).(j)
    done
  done;
  b

let diff_count a b =
  let count = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if Char.(a.(i).(j) <> b.(i).(j)) then incr count
    done
  done;
  !count

let ans =
  Iter.(1 -- 4)
  |> Iter.fold 
    (fun (ans, s) i ->
      let s = rotate90 s in
      let count = diff_count s t in
      min ans (count + i % 4), s
    )
    (n * n + 5, s)
  |> fst

let () = printf "%d\n%!" ans
