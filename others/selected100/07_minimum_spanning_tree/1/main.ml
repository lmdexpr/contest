open Scanf
open Printf

module Union_find = struct
  [@@@warnerror "-unused-field"]
  type 'a root = { mutable value : 'a ; mutable rank : int }

  type 'a t = { mutable node : 'a node }
  and 'a node =
    | Inner of 'a t
    | Root of 'a root

  let create v = { node = Root { value = v; rank = 0 } }

  let rec compress t ~inner_node ~inner ~descendants =
    match t.node with
    | Root r ->
      List.iter (fun t -> t.node <- inner_node) descendants;
      t, r
    | Inner t' as node ->
      compress t' ~inner_node:node ~inner:t ~descendants:(inner :: descendants)

  let representative t =
    match t.node with
    | Root r -> t, r
    | Inner t' as node -> compress t' ~inner_node:node ~inner:t ~descendants:[]

  let root t =
    match t.node with
    | Root r -> r
    | _ -> snd (representative t)

  let same_class t1 t2 = (root t1) == (root t2)

  let union t1 t2 =
    let t1, r1 = representative t1 in
    let t2, r2 = representative t2 in
    if r1 == r2
    then ()
    else (
      let n1 = r1.rank in
      let n2 = r2.rank in
      if n1 < n2
      then t1.node <- Inner t2
      else (
        t2.node <- Inner t1;
        if n1 = n2 then r1.rank <- r1.rank + 1))
end

let n, m = scanf "%d %d" @@ fun n m -> n, m

type edge = {u : int; v : int; w : int;}

let es = Array.init m (fun _ -> scanf " %d %d %d" @@ fun u v w -> {u; v; w;})

(* kruskal / クラスカル法 *)
let ans =
  let dsu = Array.init n Union_find.create in

  Array.sort (fun e e' -> compare e.w e'.w) es;
  es
  |> Array.fold_left (fun ans {u; v; w;} ->
    if Union_find.same_class dsu.(u) dsu.(v) then ans
    else (
      Union_find.union dsu.(u) dsu.(v);
      ans + w
    )
  ) 0

let () = printf "%d\n%!" ans
