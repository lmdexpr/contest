open Core

let n = Scanf.scanf "%d" ident
let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let inf = n + 1
let longest_increasing_subsequence ~n indices =
  let result = Array.init n ~f:(const 0) in
  let b = Array.init n ~f:(const inf) in
  Iter.fold
    (fun k i ->
       let cnt =
         Option.value_exn (
           Array.binary_search ~len:(k+1) ~compare b `First_greater_than_or_equal_to a.(i)
         )
       in
       b.(cnt) <- a.(i);
       result.(i) <- cnt + 1;
       k + Bool.to_int (cnt = k)
    )
    0
    indices
  |> ignore;
  result

let p = longest_increasing_subsequence ~n Iter.(0 -- (n - 1))
let q = longest_increasing_subsequence ~n Iter.((n - 1) --^ 0)

let () =
  Iter.(0 -- (n - 1))
  |> Iter.map (fun i -> p.(i) + q.(i) - 1)
  |> Iter.max_exn ~lt:(<)
  |> printf "%d\n%!"
