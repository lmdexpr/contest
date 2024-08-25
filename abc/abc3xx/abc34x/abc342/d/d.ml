open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let divide_by_squares n =
  let sqrt = Float.sqrt (float n) |> Float.to_int in
  Iter.(2 -- sqrt)
  |> Iter.fold
    (fun n p ->
      let rec loop n =
        if n % p <> 0 || (n / p) % p <> 0 then n
        else
          loop (n / p / p)
      in
      loop n
    ) n

let memo = Array.create ~len:200005 0L
open Int64
let () =
  Array.iter a ~f:(fun a ->
    let a = divide_by_squares a in
    memo.(a) <- memo.(a) + 1L
  )

let ans = memo.(0) * (of_int n - 1L)
let ans =
  Iter.(1 -- 200004)
  |> Iter.map  Int64.(fun i -> memo.(i) * (memo.(i) - 1L) / 2L)
  |> Iter.fold Int64.(+) ans

let () = printf "%Ld\n%!" ans
