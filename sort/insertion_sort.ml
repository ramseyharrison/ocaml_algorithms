let insert (num : int) (numbers : int list) : int list =
  let rec loop (numbers : int list) =    
    match numbers with
    |[] -> [num]
    |first :: tl ->
      if num < first then first :: (loop tl)
      else num :: first :: tl  in
  loop numbers

let sort(numbers : int list) =
  let rec loop (numbers : int list) (sorted : int list) =
    match numbers with
    |hd :: tl -> loop tl (insert hd sorted)      
    |_ -> List.rev sorted
  in  
  loop numbers [] 
        

                      
