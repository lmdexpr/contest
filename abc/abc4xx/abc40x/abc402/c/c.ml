open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let kinds = Array.create ~len:m 0
let foods = Array.create ~len:n []
let () =
  for i = 0 to m - 1 do
    kinds.(i) <- scanf " %d" Fn.id;
    for _ = 0 to kinds.(i) - 1 do
      scanf " %d" @@ fun x ->
      foods.(x - 1) <- i :: foods.(x - 1)
    done
  done

let () =
  let ans = ref 0 in
  for _ = 0 to n - 1 do
    let b  = scanf " %d" Fn.id in
    foods.(b - 1)
    |> List.iter ~f:(fun i ->
      kinds.(i) <- kinds.(i) - 1;
      if kinds.(i) = 0 then
        ans := !ans + 1
    );
    printf "%d\n" !ans
  done
