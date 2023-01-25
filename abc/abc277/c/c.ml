open Core

let n = Scanf.scanf "%d" ident

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

module SI = Set.Make(Int)

let rec bfs s =
  if Queue.is_empty queue then s
  else
    match Queue.dequeue_exn queue |> Hashtbl.find ladder with
    | None -> s
    | Some around ->
      let f s i = 
        if SI.mem s i then s
        else begin
          Queue.enqueue queue i;
          SI.add s i
        end
      in
      let s = List.fold around ~init:s ~f in
      bfs s

let () = bfs SI.empty |> SI.max_elt |> Option.value ~default:1 |> printf "%d\n" 
