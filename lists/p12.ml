type 'a rle = One of 'a | Many of int * 'a

let decode_rle l =
  let rec handle_many acc x = function
    | 0 -> acc
    | count -> handle_many (x :: acc) x (count - 1)
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: tail -> aux (x :: acc) tail
    | Many (count, x) :: tail -> aux (handle_many acc x count) tail
  in
  List.rev @@ aux [] l
