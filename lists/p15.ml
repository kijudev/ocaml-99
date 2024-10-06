let duplicate list n =
  let rec prepend x acc = function
    | 0 -> acc
    | n -> prepend x (x :: acc) (n - 1)
  in
  let rec aux acc = function
    | [] -> acc
    | x :: tail -> aux (prepend x acc n) tail
  in
  List.rev @@ aux [] list
