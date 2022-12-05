open Core

let n = Scanf.scanf "%d" ident

module SS = Set.Make(String)

let () =
  Iter.(1 -- n)
  |> Iter.fold (fun (set, acc) i ->
      let s = Scanf.scanf " %s" ident in
      if Set.mem set s then set, acc
      else
        Set.add set s, i :: acc
    ) (SS.empty, [])
  |> Tuple2.get2
  |> List.rev
  |> List.iter ~f:(printf "%d\n")
