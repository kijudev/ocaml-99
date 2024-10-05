let rec nth i = function
  | [] -> None
  | h :: t -> if i = 0 then Some h else nth (i - 1) t
