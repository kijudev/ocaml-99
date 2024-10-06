type 'a rle = One of 'a | Many of int * 'a

let encode list =
  let advance count x = if count = 1 then One x else Many (count, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> advance (count + 1) x :: acc
    | x :: y :: tail ->
        if x = y then aux (count + 1) acc (y :: tail)
        else aux 0 (advance (count + 1) x :: acc) (y :: tail)
  in
  List.rev @@ aux 0 [] list
