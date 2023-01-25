open Core

let n = Scanf.scanf "%d" ident

let parentheses_of_int i = String.init n ~f:(fun j -> if i land (1 lsl j) = 0 then '(' else ')') |> String.rev

let correct_parentheses p =
  let open Option in
  let f c acc =
    let acc = acc + Char.(if c = '(' then 1 else -1) in
    some_if (acc >= 0) acc
  in
  String.fold p ~init:(Some 0) ~f:(fun acc c -> acc >>= f c)
  |> Option.value_map ~default:false ~f:Int.(equal 0)

let () =
  for i = 0 to (1 lsl n) - 1 do
    let p = parentheses_of_int i in

    if correct_parentheses p then
      print_endline p
  done
