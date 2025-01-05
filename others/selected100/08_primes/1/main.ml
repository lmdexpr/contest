open Core
open Scanf

let eratosthenes n =
  let sieve = Array.init n ~f:(const 1) in
  let rec eratosthenes ?(acc=Iter.empty) = function
    | []      -> acc
    | x :: xs ->
      if sieve.(x) = 0 then eratosthenes ~acc xs
      else begin
        List.range ~start:`inclusive ~stop:`inclusive ~stride:x (x*x) (n - 1)
        |> List.iter ~f:(fun x -> sieve.(x) <- 0);
        eratosthenes ~acc:(Iter.snoc acc x) xs
      end
  in
  let primes = eratosthenes List.(range 2 n) |> Iter.to_array in
  for i = 1 to Array.length sieve - 1 do
    sieve.(i) <- sieve.(i) + sieve.(i - 1);
  done;
  primes, sieve

let n = scanf "%d" Fn.id
let sqrt_n = Int.of_float @@ Float.sqrt @@ Float.of_int n

let primes, _ = eratosthenes sqrt_n

let () = 
  let n = ref n in
  printf "%d:" !n;
  Array.iter primes ~f:(fun p ->
    while !n mod p = 0 do
      printf " %d" p;
      n := !n / p;
    done
  );
  if !n > 1 then
    printf " %d" !n;
  printf "\n%!"
