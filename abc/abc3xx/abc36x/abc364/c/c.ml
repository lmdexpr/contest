open Core
open Scanf

let n, x, y = scanf "%d %Ld %Ld" Tuple3.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)
let b = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let () =
  Array.sort a ~compare:Int64.descending;
  Array.sort b ~compare:Int64.descending

let count limit =
  Array.fold_until ~init:(0, 0L) 
    ~f:(fun (count, sum) a ->
      let count = count + 1 in
      let sum = Int64.(sum + a) in
      if Int64.(sum <= limit) then 
        Continue (count, sum)
      else 
        Stop count
    )
    ~finish:fst

let ans = min (count x a) (count y b)

let () = printf "%d\n%!" ans
