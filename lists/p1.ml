let rec last = function
  | [] -> None
  | [ head ] -> Some head
  | head :: tail -> last tail

let l1 = []
let l2 = [ 1; 2; 3; 4 ]
let l3 = [ 'x'; 'y'; 'z' ]
