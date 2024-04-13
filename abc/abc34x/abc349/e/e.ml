open Core
open Scanf

let a  = Array.init 3 ~f:(fun _ -> Array.init 3 ~f:(const 0L))
let () =
  scanf "%Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld %Ld" 
    (fun a00 a01 a02 a10 a11 a12 a20 a21 a22 ->
    a.(0).(0) <- a00; a.(0).(1) <- a01; a.(0).(2) <- a02;
    a.(1).(0) <- a10; a.(1).(1) <- a11; a.(1).(2) <- a12;
    a.(2).(0) <- a20; a.(2).(1) <- a21; a.(2).(2) <- a22)



let takahashi_won = true

let ans = if takahashi_won then "Takahashi" else "Aoki"

let () = printf "%s\n%!" ans
