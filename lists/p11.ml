type 'a rle = One of 'a | Many of int * 'a

let encode_rle l =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | x :: y :: tail ->
        if x = y then aux (count + 1) acc (y :: tail)
        else aux 0 ((count + 1, x) :: acc) (y :: tail)
  in
  List.rev @@ aux 0 [] l

let transform_rle l =
  let rec aux acc = function
    | [] -> acc
    | (1, x) :: tail -> aux (One x :: acc) tail
    | (count, x) :: tail -> aux (Many (count, x) :: acc) tail
  in
  List.rev @@ aux [] l

let modified_rle l = encode_rle l |> transform_rle
