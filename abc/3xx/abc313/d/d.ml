open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let ask v =
  printf "?";
  Array.iter v ~f:(fun v -> printf " %d" @@ v + 1);
  printf "\n%!";
  scanf " %d" Fn.id

let ans = Array.init n ~f:(const 0)
let r =
  Iter.(0 -- k) |> Iter.fold
  (fun r i ->
    let v =
      Iter.(0 -- k) |> Iter.filter ((<>) i) |> Iter.to_array
    in
    ans.(i) <- ask v;
    r lxor ans.(i)
  ) 0

let () =
  for i = 0 to k do
    ans.(i) <- r lxor ans.(i)
  done

let v = Array.init k ~f:Fn.id
let s = Iter.(0 -- (k - 2)) |> Iter.map (Array.get ans) |> Iter.fold (lxor) 0
let () =
  for i = k + 1 to n - 1 do
    v.(k - 1) <- i;
    ans.(i) <- s lxor ask v
  done

let () =
  printf "!";
  Array.iter ans ~f:(printf " %d");
  printf "\n%!";
