open Core

let n, a, b = Scanf.scanf "%d %Ld %Ld" Tuple3.create
let s = Scanf.scanf " %s" ident

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.map (fun pos ->
      let until_palindrome =
        let pos i = (i + pos) % n in
        Iter.(0 -- (n / 2 - 1))
        |> Iter.filter_count Char.(fun i -> s.[pos i] <> s.[pos @@ n - 1 - i])
        |> Int64.of_int
      in
      Int64.(a * of_int pos + b * until_palindrome)
    )
  |> Iter.min_exn ~lt:Int64.(<)

let () = printf "%Ld\n%!" ans
