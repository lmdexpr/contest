open Core

let t = Scanf.scanf "%d" ident

let test () =
  let n = Scanf.scanf " %d" ident in
  Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)
  |> Array.count ~f:(fun a -> a % 2 <> 0)

let () =
  for _ = 1 to t do
    printf "%d\n" @@ test ()
  done
