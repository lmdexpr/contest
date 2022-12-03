open Core

let h, _w = Scanf.scanf "%d %d" Tuple2.create

let () =
  List.init h ~f:(fun _ -> Scanf.scanf " %s" ident)
  |> List.map ~f:(String.count ~f:Char.(equal '#'))
  |> List.sum (module Int) ~f:ident
  |> printf "%d\n%!"
