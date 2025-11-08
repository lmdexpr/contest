open Core
open Scanf

let ch f a i v = a.(i) <- f a.(i) v

let n = scanf " %d" Fn.id

let ps = Dynarray.create ()
let sumw = ref 0
let sumb = ref 0L
let () =
  for _ = 1 to n do
    scanf " %d %Ld %Ld" @@ fun w h b ->

    if Int64.(b < h) then
      Dynarray.add_last ps (w, Int64.(h - b));

    sumw := !sumw + w;
    sumb := Int64.(!sumb + b);
  done

let sumw = !sumw / 2
let sumb = !sumb

let dp = Array.create ~len:(sumw + 1) 0L
let () =
  ps |> Dynarray.iter @@ fun (w, v) ->

  for j = sumw - w downto 0 do
    ch Int64.max dp (j + w) Int64.(dp.(j) + v)
  done

let ans = Int64.(dp.(sumw) + sumb)

let () = printf "%Ld\n" ans
