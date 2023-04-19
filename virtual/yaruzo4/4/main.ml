open Core
open Scanf

let x = scanf "%Ld" ident

open Int64
let modulo = 998244353L
let( *% ) a b = (a * b) % modulo

module Memo = struct
  include Memo
  let recursive f =
    let h = Hashtbl.create (module Int64) in
    let rec g x =
      match Hashtbl.find h x with
      | Some v -> v
      | None ->
        let y = f g x in
        Hashtbl.add_exn h ~key:x ~data:y;
        y
    in
    g
end

let solve ?(acc=1L) self = function
  | 1L -> acc
  | 2L -> acc *% 2L
  | 3L -> acc *% 3L
  | 4L -> acc *% 4L
  | n when n % 2L = 0L ->
    let l = self (n / 2L) in
    acc *% l *% l
  | n  ->
    let l = self (n / 2L) in
    let r = self (n / 2L + 1L) in
    acc *% l *% r

let ans = Memo.recursive solve x

let () = printf "%Ld\n%!" ans
