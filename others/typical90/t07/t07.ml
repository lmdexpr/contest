open Core

let n = Scanf.scanf "%d" ident
let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)
let () = Array.sort a ~compare:Int.compare

let q = Scanf.scanf " %d" ident
let b = Array.init q ~f:(fun _ -> Scanf.scanf " %d" ident)

let () =
  Array.iter b ~f:(fun b ->
      let pos =
        Array.binary_search a ~compare `First_strictly_greater_than b
        |> Option.value ~default:(n-1)
      in
      let ans1 = abs @@ a.(pos) - b
      and ans2 = abs @@ a.((pos-1) % n) - b in
      printf "%d\n%!" @@ min ans1 ans2
    )
