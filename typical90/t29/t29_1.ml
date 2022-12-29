open Core

let w, n = Scanf.scanf "%d %d" Tuple2.create

let height = Array.init w ~f:(const 0)

let () =
  for _ = 1 to n do
    let l, r = Scanf.scanf " %d %d" @@ fun l r -> l - 1, r - 1 in
    let higher =
      Iter.(l -- r)
      |> Iter.map (Array.get height)
      |> Iter.max ~lt:(<)
      |> Option.value ~default:0
    in
    let higher = higher + 1 in
    for i = l to r do
      height.(i) <- higher
    done;
    printf "%d\n" higher
  done
