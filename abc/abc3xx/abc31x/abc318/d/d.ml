open Core
open Scanf

let n = scanf "%d" Fn.id

let e = Array.make_matrix ~dimx:n ~dimy:n 0
let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      e.(i).(j) <-
        if i < j then scanf " %d" Fn.id
        else if i = j then 0
        else e.(j).(i)
    done
  done

let dfw x = 
  let dp = Array.create ~len:(1 lsl n) (-1) in
  dp.(0) <- 0;
  let rec dfw x =
    if dp.(x) < 0 then
      Iter.(0 -- (n - 1)) |> Iter.filter (fun i -> x land (1 lsl i) <> 0)
      |> Iter.iter (fun i ->
        Iter.((i + 1) -- (n - 1)) |> Iter.filter (fun j -> x land (1 lsl j) <> 0)
        |> Iter.iter (fun j ->
          dp.(x) <- max dp.(x) (dfw (x - (1 lsl i) lxor (1 lsl j)) + e.(i).(j))
        )
      );
    dp.(x)
  in
  dfw x

let ans =
  if n % 2 = 0 then dfw (1 lsl n - 1)
  else
    let bits = 1 lsl n - 1 in
    Iter.(0 -- (n - 1))
    |> Iter.map (fun i -> dfw (bits - (1 lsl i)))
    |> Iter.max_exn ~lt:(<)

let () = printf "%d\n%!" ans
