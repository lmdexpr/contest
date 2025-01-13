open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create

let es = Array.init m ~f:(fun _ -> scanf " %d %d" @@ fun a b -> a-1, b-1)

let uf    = Array.init n ~f:Union_find.create
let get x = Union_find.get uf.(x)

let cnt = Array.create ~len:n 1L
let ans = Array.create ~len:m 0L
let () =
  let n = Int64.of_int n in

  ans.(m - 1) <- Int64.(n * (n - 1L) / 2L);

  for i = m - 1 downto 1 do
    let a, b = es.(i) in

    if Union_find.same_class uf.(a) uf.(b) then 
      ans.(Int.pred i) <- ans.(i)
    else Int64.(
      ans.(Int.pred i) <- ans.(i) - cnt.(get a) * cnt.(get b);
      let sum  = cnt.(get a) + cnt.(get b) in
      Union_find.union uf.(a) uf.(b);
      cnt.(get a) <- sum;
    )
  done

let () = 
  for i = 0 to m - 1 do
    printf "%Ld\n" ans.(i)
  done
