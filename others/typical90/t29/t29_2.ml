open Core

let w, n = Scanf.scanf "%d %d" Tuple2.create

let l = Array.init n ~f:(const 0)
let r = Array.init n ~f:(const 0)

module SI = Set.Make(Int)
let compression =
  Iter.(0 -- (n - 1))
  |> Iter.fold
    (fun acc i ->
       let li, ri = Scanf.scanf " %d %d" @@ fun l r -> l - 1, r - 1 in
       l.(i) <- li;
       r.(i) <- ri;
       li :: ri :: acc
    )
    []
  |> List.sort ~compare
  |> List.remove_consecutive_duplicates ~equal
  |> List.to_array

let () =
  for i = 0 to n - 1 do
    let default = Array.last compression in
    l.(i) <- Array.binary_search compression ~compare `First_greater_than_or_equal_to l.(i) |> Option.value ~default;
    r.(i) <- Array.binary_search compression ~compare `First_greater_than_or_equal_to r.(i) |> Option.value ~default
  done

let height = Array.init (Array.length compression) ~f:(const 0)

let () =
  for i = 0 to n - 1 do
    let l = l.(i) and r = r.(i) in
    let higher = Iter.(l -- r) |> Iter.map (Array.get height) |> Iter.fold max 0 in
    let higher = higher + 1 in
    for i = l to r do
      height.(i) <- higher
    done;
    printf "%d\n" higher
  done
