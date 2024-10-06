let drop list n =
  let rec aux count acc = function
    | [] -> acc
    | x :: tail ->
        if count = 1 then aux n acc tail else aux (count - 1) (x :: acc) tail
  in
  if n = 0 then [] else List.rev @@ aux n [] list
