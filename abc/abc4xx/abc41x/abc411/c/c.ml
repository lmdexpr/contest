open Core
open Scanf

let n = scanf " %d" Fn.id
let q = scanf " %d" Fn.id

let m = Array.create ~len:n false
let ans = ref 0

let () =
  for _ = 1 to q do
    scanf " %d" @@ fun a ->
    let a = a - 1 in

    m.(a) <- not m.(a);

    if n = 1 then (
      ans := Bool.to_int m.(0);
    ) else if a <= 0 then (
      let right = m.(a + 1) in
      if m.(a) && not right then incr ans;
      if not m.(a) && not right then decr ans;
    ) else if n-1 <= a then (
      let left = m.(a - 1) in
      if not left && m.(a) then incr ans;
      if not left && not m.(a) then decr ans;
    ) else (
      let left  = m.(a - 1) in
      let right = m.(a + 1) in

      if m.(a) then (
        if left && right then (
          ans := !ans - 1
        );
        if not left && not right then
          incr ans;
      ) else (
        if left && right then (
          ans := !ans + 1
        );
        if not left && not right then
          decr ans;
      )
    );

    printf "%d\n%!" !ans
  done
