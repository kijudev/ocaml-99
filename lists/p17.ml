let split list n =
  let rec aux pos acc = function
    | [] -> acc
    | x :: tail ->
        let left, right = acc in
        if pos <= n then aux (pos + 1) (x :: left, right) tail
        else aux (pos + 1) (left, x :: right) tail
  in
  let right, left = aux 1 ([], []) list in
  (List.rev right, List.rev left)
