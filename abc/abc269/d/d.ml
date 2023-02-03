open Core
open Scanf

let n = scanf "%d" ident
let pos = Array.init n ~f:(fun _ -> scanf " %d %d" @@ fun x y -> x + 1000, y + 1000)

let uf = Array.init n ~f:Union_find.create
let () =
  Array.iteri pos ~f:(fun i (x, y) ->
      Array.iteri pos ~f:(fun j (z, w) ->
          if
            (z = x - 1 && w = y - 1) ||
            (z = x - 1 && w = y    ) ||
            (z = x     && w = y - 1) ||
            (z = x     && w = y + 1) ||
            (z = x + 1 && w = y    ) ||
            (z = x + 1 && w = y + 1)
          then
            Union_find.union uf.(i) uf.(j)
        )
    )

module SI = Set.Make(Int)
let ans = Array.map uf ~f:Union_find.get |> SI.of_array |> SI.length

let () = printf "%d\n%!" ans
