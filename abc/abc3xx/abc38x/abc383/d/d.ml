open Core
open Scanf

let n = scanf "%Ld" Fn.id
let sqrt_n = Float.of_int64 n |> Float.sqrt |> Float.to_int

let primes =
  let n = sqrt_n + 1 in
  let sieve = Array.init n ~f:(const 1) in
  let rec eratosthenes ?(acc=Iter.empty) = function
    | []      -> acc
    | x :: xs ->
      if sieve.(x) = 0 then eratosthenes ~acc xs
      else begin
        List.range ~start:`inclusive ~stop:`inclusive ~stride:x (x*x) (n - 1)
        |> List.iter ~f:(fun x -> sieve.(x) <- 0);
        eratosthenes ~acc:(Iter.snoc acc @@ Int64.of_int x) xs
      end
  in
  eratosthenes List.(range 2 n)
  |> Iter.to_array

let len = Array.length primes

let ans0 =
  Iter.(0 -- (len - 1))
  |> Iter.map (fun i -> primes.(i))
  |> Iter.take_while Int64.(fun p -> p * p * p * p * p * p * p * p <= n)
  |> Iter.length

let ans1 =
  0

let ans = ans0 + ans1

let () = printf "%d\n%!" ans
