open Core
open Scanf

let n = scanf "%d" Fn.id

let ladder = Hashtbl.create ~size:(2 * n) (module Int)

let () =
  for _ = 1 to n do
    let a, b = Scanf.scanf " %d %d" @@ fun a b -> a, b in
    let f v = function
      | None     -> [ v ]
      | Some acc -> v :: acc
    in
    Hashtbl.update ladder a ~f:(f b);
    Hashtbl.update ladder b ~f:(f a)
  done

let queue = Queue.singleton 1

let ans = ref Int.Set.empty
let () =
  while not @@ Queue.is_empty queue do
    match Queue.dequeue_exn queue |> Hashtbl.find ladder with
    | None -> ()
    | Some around ->
      around |> List.iter ~f:(fun i ->
          if not @@ Set.mem !ans i then begin
            Queue.enqueue queue i;
            ans := Set.add !ans i
          end)
  done;
  !ans |> Set.max_elt |> Option.value ~default:1 |> printf "%d\n"
