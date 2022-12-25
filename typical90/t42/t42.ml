open Core

let k = Scanf.scanf "%d" ident

let modulo = 1000000007

let memo = Hashtbl.create ~size:k (module Int)
let rec dp = function
  | 0 -> 1
  | 1 -> 1
  | 2 -> 2
  | 3 -> 4
  | 4 -> 8
  | 5 -> 16
  | 6 -> 32
  | 7 -> 64
  | 8 -> 128
  | k ->
    match Hashtbl.find memo k with
    | Some v -> v
    | None ->
      let v = Iter.(1 -- 9) |> Iter.fold (fun acc i -> (acc + dp (k - i)) % modulo) 0 in
      Hashtbl.add_exn memo ~key:k ~data:v;
      v

let () = printf "%d\n%!" @@ if k % 9 <> 0 then 0 else dp k
