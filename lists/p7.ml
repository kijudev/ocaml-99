type 'a node = One of 'a | Many of 'a node list

let flatten l =
  let rec aux acc = function
    | [] -> acc
    | One x :: tail -> aux (x :: acc) tail
    | Many l :: tail -> aux (aux acc l) tail
  in
  List.rev (aux [] l)
