open Core

let rec digit_sum ?(acc=0) x =
  if x <= 0 then acc
  else
    let acc = acc + x % 10 in
    digit_sum ~acc (x / 10)

let n, k = Scanf.scanf "%d %d" Tuple2.create

let modulo = 100000

let next = Array.init modulo ~f:(fun i -> (i + digit_sum i) % modulo)
let time_stamp = Array.init modulo ~f:(const @@ -1)
let rec simulate ?(count=0) t =
  if time_stamp.(t) <> -1 then count, t
  else begin
    time_stamp.(t) <- count;
    simulate ~count:(count + 1) next.(t)
  end
let count, start_cycle = simulate n
let start_cycle = time_stamp.(start_cycle)

let k =
  if k < start_cycle then k
  else
    start_cycle + (k - start_cycle) % (count - start_cycle)

let () =
  Iter.((modulo - 1) --^ 0)
  |> Iter.find_pred_exn (fun i -> time_stamp.(i) = k)
  |> printf "%d\n%!"
