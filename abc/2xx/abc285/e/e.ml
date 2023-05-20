open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %Ld" ident)

let dp = Array.init (n + 1) ~f:(const 0L)

let () =
  Iter.(0 -- n) |> Iter.map (fun i -> a.(i / 2)) |> Iter.scan Int64.(+) 0L |> Iter.cons 0L
  |> Iter.iteri (fun i consecutive ->
      for j = i to n do
        let pd = dp.(j - i) in
        dp.(j) <- Int64.(max dp.(j) @@ pd + consecutive)
      done
    )

let () = printf "%Ld\n%!" dp.(n)
