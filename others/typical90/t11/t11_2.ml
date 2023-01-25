open Core

let n = Scanf.scanf "%d" ident
let tasks = Array.init n ~f:(fun _ -> Scanf.scanf " %d %d %d" Tuple3.create)

let () = Array.sort tasks ~compare:(fun (dl, _, _) (dr, _, _) -> compare dr dl)

let () =
  Iter.((1 lsl n) - 1 --^ 0)
  |> Iter.map (fun mask ->
      Iter.(0 -- (n - 1))
      |> Iter.fold_while (fun acc i ->
          let time, money = acc in
          if mask land (1 lsl i) = 0 then acc, `Continue
          else
            let i = n - 1 - i in
            let d, c, s = tasks.(i) in
            if time + c > d then acc, `Stop
            else
              (time + c, money + s), `Continue
        ) (0, 0)
      |> Tuple2.get2
    )
  |> Iter.max_exn ~lt:Int.(<)
  |> printf "%d\n%!"
