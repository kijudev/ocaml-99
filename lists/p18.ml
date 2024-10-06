let slice list i k =
  let rec aux pos acc = function
    | [] -> acc
    | x :: tail ->
        if pos >= i && pos <= k then aux (pos + 1) (x :: acc) tail
        else aux (pos + 1) acc tail
  in
  List.rev @@ aux 0 [] list
