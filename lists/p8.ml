let rec compress = function
  | x :: y :: tail ->
      if x = y then compress (y :: tail) else x :: compress (y :: tail)
  | l -> l
