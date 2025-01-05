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

let primes, _ = eratosthenes 100_005
let primes    = Set.of_array (module Int) primes

let a = Array.init 100_005 ~f:(fun i -> 
  Bool.to_int (Set.mem primes i && Set.mem primes ((i + 1) / 2))
)

let cumsum a =
  let paired f a b = let r = f a b in r, r in
  Array.folding_map a ~init:0 ~f:(paired Int.(+))

let cumsum = cumsum a

let q = scanf "%d" Fn.id

let () =
  for _ = 1 to q do
    let l, r = scanf " %d %d" Tuple2.create in
    printf "%d\n" @@ cumsum.(r) - cumsum.(l - 1)
  done
