open Printf
open Scanf

module Seq = struct
  include Seq

  let rec range start stop () =
    if start >= stop then Seq.Nil
    else
      Seq.Cons (start, range (start + 1) stop)

  let rec find f seq = match seq () with
    | Seq.Nil                  -> None
    | Seq.Cons (x, _) when f x -> Some x
    | Seq.Cons (_, xs)         -> find f xs

  let forever f =
    let rec loop () = Seq.Cons (f (), loop) in
    loop

  let rec take_while f seq () = match seq () with
    | Seq.Nil                   -> Seq.Nil
    | Seq.Cons (x, xs) when f x -> Seq.Cons (x, take_while f xs)
    | Seq.Cons _                -> Seq.Nil
end

let run_length_compress a =
  let n = Array.length a in
  let rec loop i now (len, acc) =
    if n <= i then (now, len) :: acc
    else
      loop (i + 1) a.(i) @@
      if now = a.(i) then len + 1, acc
      else
        1, (now, len) :: acc
  in
  if n = 0 then []
  else
    List.rev @@ loop 1 a.(0) (1, [])

let find_max_repeats a =
  run_length_compress a
  |> List.fold_left
    (fun (acc, (s, m)) (_, l) ->
      if m < l then acc + l, (acc, l)
      else
        acc + l, (s, m)
    )
    (0, (0, 0))
  |> snd

let solve h stones =
  let step () =
    let score = ref 0 in
    for i = 0 to h - 1 do
      match find_max_repeats stones.(i) with
      |     _, len when len < 3 -> ()
      | start, len ->
        score := !score + stones.(i).(start) * len;
        for j = start to start + len - 1 do
          stones.(i).(j) <- 0
        done;
    done;
    for i = h - 2 downto 0 do
      for j = 0 to 4 do
        let k =
          Seq.range (i + 1) h
          |> Seq.find (fun k -> stones.(k).(j) > 0)
          |> Option.value ~default:h
        in
        if stones.(k-1).(j) = 0 then (
          stones.(k-1).(j) <- stones.(i).(j);
          stones.(i).(j)   <- 0
        );
      done;
    done;
    !score
  in
  Seq.forever step
  |> Seq.take_while (fun score -> score > 0)
  |> Seq.fold_left (+) 0
  |> printf "%d\n%!"

let input_int _ = scanf " %d" @@ fun x -> x

let rec loop () =
  match input_int () with
  | 0 -> ()
  | h ->
    solve h @@ Array.init h (fun _ -> Array.init 5 input_int);
    loop ()

let () = loop ()
