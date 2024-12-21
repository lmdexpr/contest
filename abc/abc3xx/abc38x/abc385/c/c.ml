open Core
open Scanf

let n = scanf "%d" Fn.id
let h = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let tbl = Hashtbl.create (module Int)

let () =
  Array.iteri h ~f:(fun i x ->
    Hashtbl.update tbl x ~f:(function
      | None   -> Int.Set.singleton i
      | Some s -> Set.add s i
    )
  )

let ans =
  Hashtbl.fold tbl ~init:1 ~f:(fun ~key:_ ~data acc ->
    let a = Set.to_array data in
    let n = Array.length a in

    Iter.(0 -- (n - 1))
    |> Iter.fold (fun acc i ->
      Iter.((i + 1) -- (n - acc))
      |> Iter.fold (fun acc j ->
        let diff = a.(j) - a.(i) in
        Iter.iterate ((+) diff) a.(j)
        |> Iter.take_while (fun x -> Set.mem data x)
        |> Iter.length
        |> (+) 1
        |> Int.max acc
      ) acc
    ) acc
  )

let () = printf "%d\n%!" ans
