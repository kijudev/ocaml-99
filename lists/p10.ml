let rle l =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | x :: y :: tail ->
        if x = y then aux (count + 1) acc (y :: tail)
        else aux 0 ((count + 1, x) :: acc) (y :: tail)
  in
  List.rev @@ aux 0 [] l
