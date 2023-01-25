open Core

let n, k, p = Scanf.scanf "%d %d %d" Tuple3.create

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let enumeration ?(padding=0) num =
  let result = Array.init (k+1) ~f:(const []) in
  Iter.(0 -- ((1 lsl num) - 1))
  |> Iter.iter (fun i ->
      let enums = 
        Iter.(0 -- (num - 1))
        |> Iter.filter (fun j -> i land (1 lsl j) <> 0)
      in
      let count = Iter.length enums in
      if count <= k then
        let price = Iter.fold (fun price j -> price + a.(padding + j)) 0 enums in
        result.(count) <- price :: result.(count)
    );
  Array.map result ~f:(fun x -> List.sort x ~compare |> Array.of_list)

let prices1 = enumeration (n/2)
and prices2 = enumeration ~padding:(n/2) (n - n / 2)

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let (left, right) = if ok mid then (left, mid) else (mid, right) in
    binsearch ~ok left right

let () =
  Array.foldi prices1 ~init:0L ~f:(fun count acc ->
      Array.fold ~init:acc ~f:(fun acc p1 ->
          let p2 = prices2.(k - count) in
          let count = binsearch ~ok:(fun i -> p1 + p2.(i) > p) (-1) (Array.length p2) in
          Int64.(acc + of_int count)
        )
    )
  |> printf "%Ld\n%!"
