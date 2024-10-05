let pack l =
  let rec aux curr acc = function
    | [] -> acc
    | [ x ] -> (x :: curr) :: acc
    | x :: y :: tail ->
        if x = y then aux (x :: curr) acc tail
        else aux [] ((x :: curr) :: acc) tail
  in
  List.rev (aux [] [] l)
