open Core
open Scanf

let n = scanf "%d" Fn.id
let h = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans = Array.create ~len:n 0
let () =
  let rec next i len = function
    | []      -> 0, []
    | x :: xs -> if h.(x) < h.(i + 1) then next i (len - 1) xs else len, x :: xs
  in
  Iter.((n - 2) --^ 0)
  |> Iter.fold (fun (stack, len) i ->
    let len, stack = next i len stack in
    let len = len + 1 in
    ans.(i) <- len;
    (i+1) :: stack, len
  ) ([], 0)
  |> ignore

let () = Array.iter ans ~f:(printf "%d "); printf "\n%!"
