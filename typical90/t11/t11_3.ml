open Core

let n = Scanf.scanf "%d" ident
let tasks = Array.init n ~f:(fun _ -> Scanf.scanf " %d %d %d" Tuple3.create)

let () = Array.sort tasks ~compare:(fun (dl, _, _) (dr, _, _) -> compare dl dr)

let answer =
  let init = Array.init 5001 ~f:(const 0) in
  let dp now (d, c, s) =
    Array.init 5001 ~f:(fun i ->
        if i < c || i > d then now.(i)
        else
          max now.(i) @@ now.(i-c) + s
      )
  in
  Array.fold tasks ~init ~f:dp
  |> Array.max_elt ~compare:Int.compare

let () = printf "%d\n%!" @@ Option.value_exn answer
