open Core

let s = Scanf.scanf "%s" ident

let idx c = Char.to_int c - Char.to_int 'a'
let box = Array.init 300005 ~f:(const 0)

let alp = Array.init (idx 'z' + 1) ~f:(const 0)

let del n =
  for i = idx 'a' to idx 'z' do
    if box.(n) land (1 lsl i) <> 0 then
      alp.(i) <- alp.(i) - 1
  done

let f (n, acc) =
  function
  | '(' -> n + 1, acc
  | ')' ->
    del n;
    box.(n) <- 0;
    n - 1, acc
  | c   ->
    let c = idx c in
    if alp.(c) <> 0 then n, false
    else (
      box.(n) <- box.(n) lor (1 lsl c);
      alp.(c) <- alp.(c) + 1;
      n, acc
    )

let yes = String.fold ~init:(0, true) ~f s |> Tuple2.get2

let () = printf "%s\n%!" @@ if yes then "Yes" else "No"
