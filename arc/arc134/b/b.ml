open Core
open Scanf

let n = scanf "%d" ident
let s = scanf " %s" String.to_array

let ord c = Char.to_int c - Char.to_int 'a'

let indices = Array.init 26 ~f:(const Int.Set.empty)
let () =
  Array.iteri s ~f:(fun i c ->
      let ord = ord c in
      indices.(ord) <- Int.Set.add indices.(ord) i
    )

let rec greedy l r =
  printf "[%d, %d]" l r;
  if l < r then begin
    Iter.(0 -- Int.min (ord s.(l)) (ord s.(r)))
    |> Iter.mapi (fun i ord -> i, indices.(ord))
    |> Iter.filter (fun (_, s) -> not @@ Int.Set.is_empty s)
    |> Iter.head
    |> Option.value_map ~default:None ~f:(fun (i, s) -> Int.Set.max_elt s |> Option.map ~f:(fun s -> i, s))
    |> Option.iter ~f:(fun (i, q) ->
        printf " %d\n%!" q;
        indices.(i) <- Int.Set.remove indices.(i) q;
        Array.swap s l q;
        greedy (l + 1) (min q r)
      )
  end

let () = greedy 0 (n - 1)

let () = Array.iter s ~f:(printf "%c"); printf "\n%!"
