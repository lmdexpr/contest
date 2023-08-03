open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let p = Array.init n ~f:(fun _ -> [])

let () =
  for _ = 1 to m do
    let a, b = scanf " %d %d" Tuple2.create in
    let a, b = a - 1, b - 1 in
    p.(b) <- a :: p.(b);
  done

let ans =
  let f i = function
    | [] -> Some i
    | _  -> None
  in
  match Array.filter_mapi p ~f with
  | [| a |] -> a + 1
  | _       -> -1

let () = printf "%d\n%!" ans
