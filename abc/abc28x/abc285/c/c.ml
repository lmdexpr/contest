open Core

let s = Scanf.scanf "%s" ident

let ord_A = Char.to_int 'A'

let ratio = Array.init 13 ~f:(const 1L)
let () =
  for i = 1 to 12 do
    ratio.(i) <- Int64.( * ) ratio.(i - 1) 26L
  done

let () =
  String.rev s
  |> Iter.of_str
  |> Iter.map (fun c -> Char.to_int c - ord_A + 1)
  |> Iter.foldi
    (fun acc i c ->
       Int64.(acc + ratio.(i) * of_int c)
    )
    0L
  |> printf "%Ld\n%!"
