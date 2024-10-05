let rec length = function [] -> 0 | [ _ ] -> 1 | _ :: tail -> 1 + length tail

let length_tail_rec l =
  let rec aux acc = function [] -> acc | head :: tail -> aux (acc + 1) tail in
  aux 0 l
