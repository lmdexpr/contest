open Core

let n, k = Scanf.scanf "%d %d" @@ fun n k -> n, k 

let k = min n k

let a = List.init n ~f:(fun _ -> Scanf.scanf " %d" ident) |> List.rev
let a = List.drop a k @ List.init k ~f:(fun _ -> 0)

let () = List.iter a ~f:(Printf.printf "%d ")

