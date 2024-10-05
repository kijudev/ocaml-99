let reverse l =
  let rec aux acc = function
    | [] -> acc
    | head :: tail -> aux (head :: acc) tail
  in
  aux [] l
