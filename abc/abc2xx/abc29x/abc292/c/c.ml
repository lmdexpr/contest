open Core
open Scanf

let n = scanf "%d" ident

let divisor = Array.init (n+1) ~f:(const 0L)
let () =
  let (let*) x k = Iter.iter k x in
  for x = 1 to n do
    let* a = Iter.(1 -- n / x) in
    divisor.(a * x) <- Int64.succ divisor.(a * x)
  done

let ans =
  Iter.(1 -- (n - 1))
  |> Iter.map (fun i -> divisor.(i), divisor.(n - i))
  |> Iter.map Int64.(fun (ab, cd) -> ab * cd)
  |> Iter.fold Int64.(+) 0L

let () = printf "%Ld\n%!" ans
