open Core
open Scanf

let s = scanf "%s" String.to_array |> Array.map ~f:(function '?' -> -1L | '1' -> 1L | _ -> 0L)
let n = scanf " %Ld" ident

let rec to_binary ?(acc=[]) ?(len=0) = function
  | 0L -> acc, len
  | n  -> to_binary ~acc:(Int64.(n % 2L)::acc) ~len:(len+1) Int64.(n / 2L)

let bin, len = to_binary n

let ans =
  if Array.length s < len then
    Array.fold s ~init:0L
      ~f:(fun acc x -> Int64.(2L * acc + if x = -1L then 1L else x))
  else
    let rec solve ?(acc=0L) i = function
      | []    -> acc
      | _::xs ->
        let open Int64 in
        if s.(i) = -1L then
          solve Int.(i+1) ~acc:(2L * acc + 1L) xs
        else 
          solve Int.(i+1) ~acc:(2L * acc + s.(i)) xs
    in
    solve (Array.length s - len) bin

let ans = if Int64.(ans >= n) then -1L else ans

let () = printf "%Ld\n%!" ans
