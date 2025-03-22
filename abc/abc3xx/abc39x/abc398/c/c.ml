open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let tbl = Hashtbl.create (module Int)

let () = 
  Array.iter a ~f:(fun x ->
    Hashtbl.update tbl x ~f:(function
      | None   -> 1
      | Some y -> y + 1
    );
  )

let ans =
  Array.fold a ~init:(-1) ~f:(fun acc x ->
    match Hashtbl.find tbl x with
    | Some 1 -> max acc x
    | _      -> acc
  )

let ans =
  Array.findi a ~f:(fun _ x -> x = ans)
  |> Option.fold ~init:(-1) ~f:(fun _ (i, _) -> i + 1)

let () = printf "%d\n%!" ans
