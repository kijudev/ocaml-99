let rec duplicate = function [] -> [] | x :: tail -> x :: x :: duplicate tail

let duplicate_tail_rec list =
  let rec aux acc = function
    | [] -> acc
    | x :: tail -> aux (x :: x :: acc) tail
  in
  List.rev @@ aux [] list
