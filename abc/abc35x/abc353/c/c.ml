open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let () = Array.sort a ~compare:Int64.compare

let _, c =
  Iter.(0 -- (n - 1))
  |> Iter.fold (fun (r, c) i ->
    let rec calc r = 
      let r = r - 1 in
      if r <= i || Int64.(a.(r) + a.(i) < 100000000L) then r + 1
      else
        calc r
    in
    let r = calc @@ max r (i + 1) in
    r, Int64.(c + of_int Int.(n - r))
  ) (n, 0L)

let ans = Array.sum (module Int64) a ~f:Fn.id
let ans = Int64.(ans * Int.(to_int64 @@ n - 1) - c * 100_000_000L)

let () = printf "%Ld\n%!" ans
