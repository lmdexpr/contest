open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let find (i, j) =
  List.find_map [ 0, 1; 0, -1; 1, 0; -1, 0; -1, -1; -1, 1; 1, -1; 1, 1 ]
    ~f:(fun (di, dj) ->
        let open Continue_or_stop in
        String.fold_until "snuke"
          ~init:(0, [])
          ~finish:(fun (_, acc) -> Some acc)
          ~f:(fun (k, acc) c ->
              let i = i + di * k and j = j + dj * k in
              if 0 <= i && i < h && 0 <= j && j < w && Char.(s.(i).(j) = c)
              then
                Continue (k + 1, (i + 1, j + 1) :: acc)
              else
                Stop None
            )
      )
  |> Option.map ~f:List.rev

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let () =
  Iter.(
    (0 -- (h - 1)) * (0 -- (w - 1))
  )
  |> Iter.find_map find
  |> Option.iter ~f:(
    List.iter ~f:(Tuple2.uncurry @@ printf "%d %d\n")
  )
