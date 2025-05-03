open Core
open Scanf

module Array = struct
  include Array
  let cons x xs = Array.append [| x |] xs
end

let n = scanf " %d" Fn.id

let c = Array.cons 0    @@ Array.init (n - 1) ~f:(fun _ -> scanf " %d" Fn.id)
let a = Array.cons true @@ Array.init (n - 1) ~f:(fun _ -> scanf " %d" @@ fun a -> 0 < a)

let move i =
  let l i = i - c.(i) in
  if 
    Iter.(pred i --^ l i) |> Iter.find_pred Array.(get a) |> Option.is_none 
  then 
    Iter.(pred i --^ l i)
    |> Iter.min ~lt:(fun i j -> l i < l j)
    |> Option.iter ~f:(fun i ->
      a.(i) <- true;
    )

let ans =
  Iter.(pred n --^ 1) |> Iter.fold (fun ans i ->
    if not a.(i) then ans
    else (
      move i;
      ans + 1
    )
  ) 0

let () = printf "%d\n%!" ans
