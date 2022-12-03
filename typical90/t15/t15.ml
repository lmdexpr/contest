open Core

let n = Scanf.scanf "%d" ident

let m = 1000000007L

let modpow ?(m=m) a b =
  let open Int64 in
  Array.init 64 ~f:ident
  |> Array.fold ~init:(1L, a) ~f:(fun (p, q) i ->
      if b land (1L lsl i) <> 0L then p * q % m, q * q % m
      else
        p, q * q % m
    )
  |> Tuple2.get1

let div ?(m=m) a b = Int64.(a * modpow b (m - 2L) % m)

let fact    = Iter.(1 -- n) |> Iter.scan (fun p i -> Int64.(of_int i * p % m)) 1L |> Iter.to_array
let factinv = Array.init (n+1) ~f:(fun i -> div 1L fact.(i))

let comb ?(m=m) n r =
  if n < r || r < 0 then 0L
  else
    let nr = n - r in
    let open Int64 in
    (fact.(n) * factinv.(r) % m) * factinv.(nr) % m

let solve k =
  Iter.(1 -- (n / k + 1))
  |> Iter.fold (fun acc i ->
      let a = n - (k - 1) * (i - 1)
      and b = i in
      let open Int64 in
      (acc + comb a b) % m
    ) 0L
  |> Int64.to_int_exn

let () =
  for k = 1 to n do
    printf "%d\n%!" @@ solve k
  done
