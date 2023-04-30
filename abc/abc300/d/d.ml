open Core
open Scanf

let n = scanf "%Ld" ident
let isqrt n = Int64.(n |> to_float |> sqrt |> int_of_float)

let eratosthenes n =
  let sieve = Array.init n ~f:(const 1) in
  let rec eratosthenes ?(acc=Iter.empty) = function
    | []      -> acc
    | x :: xs ->
      if sieve.(x) = 0 then eratosthenes ~acc xs
      else begin
        List.range ~start:`inclusive ~stop:`inclusive ~stride:x (x*x) 300_004
        |> List.iter ~f:(fun x -> sieve.(x) <- 0);
        eratosthenes ~acc:(Iter.snoc acc @@ Int64.of_int x) xs
      end
  in
  let primes = eratosthenes List.(range 2 n) |> Iter.to_array in
  for i = 1 to Array.length sieve - 1 do
    sieve.(i) <- sieve.(i) + sieve.(i - 1);
  done;
  primes, sieve

let primes, prime_count = eratosthenes 300_005

let f a b c = Int64.(a * a * b * c * c < n)

let ans =
  List.(range 0 300)
  |> List.take_while ~f:(fun i -> f primes.(i) primes.(i) primes.(i))
  |> List.fold ~init:0 ~f:(fun acc i ->
      let a = primes.(i) in
      List.(range (i + 1) 1000)
      |> List.take_while ~f:(fun j -> f a primes.(j) primes.(j))
      |> List.fold ~init:acc ~f:(fun acc j ->
          let b = primes.(j) in
          let v = isqrt Int64.(n / (a * a * b)) in
          acc + prime_count.(v) - prime_count.(Int64.to_int_exn b)
        )
    )

let () = printf "%d\n%!" ans
