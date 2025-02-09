open Core
open Scanf

let n = scanf " %d" Fn.id

module BatAvlTree = struct
  include BatAvlTree

  (* Assume |hl - hr| < 3 を破ってしまうので NG *)
  let rec insert tree i x =
    if is_empty tree then singleton_tree x
    else
      let l = left_branch tree in
      let v = root tree in
      let r = right_branch tree in
      match height l with
      | h when i <= h   -> make_tree (insert l i x) v r
      | h when i  = h+1 -> make_tree l x (concat (singleton_tree v) r) 
      | h               -> make_tree l v (insert r (i - h - 1) x)

  let (.!()<-) = insert
end

let ans =
  Iter.(1 -- n)
  |> Iter.fold 
    BatAvlTree.(fun tree i -> scanf " %d" @@ fun p -> tree.!(p) <- i) 
    BatAvlTree.empty

let () =
  BatAvlTree.iter (printf "%d ") ans;
  printf "\n%!"
