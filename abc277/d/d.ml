open Core

let n, m = Scanf.scanf "%d %d" @@ fun n m -> n, m

let tbl = Hashtbl.create ~size:n (module Int)
let sum = ref 0
let () =
  for _ = 1 to n do
    let ai = Scanf.scanf " %d" ident in
    Hashtbl.incr tbl ai;
    sum := !sum + ai
  done
let sum = !sum

let vf = Hashtbl.to_alist tbl |> List.to_array
let () = Array.sort vf ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2)

let k  = Array.length vf

let () =
  if k = m then begin
    print_endline "0";
    exit 0
  end

let continuable i =
  let open Tuple2 in
  let x = (get1 vf.(i) + 1) % m in
  let y = get1 vf.((i + 1) % k) in
  x = y

let rec p i =
  if k <= i then 0
  else if not @@ continuable i then i
  else p (i + 1)
let p = p 0

let s = Array.init k ~f:(const 0)

let () =
  for i = 0 to k - 1 do
    let i = (p - i + k) % k in
    s.(i) <- sum;
    if continuable i then s.(i) <- s.((i + 1) % k);
    let (v, f) = vf.(i) in
    s.(i) <- s.(i) - v * f
  done

let () =
  Array.min_elt s ~compare:Int.compare 
  |> Option.value ~default:0
  |> printf "%d\n"
