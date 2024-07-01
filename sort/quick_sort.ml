let partition (numbers : int list) =
  let pivot = List.hd numbers in
  let before_pivot = List.filter (fun x -> x < pivot) numbers in
  let after_pivot = List.filter (fun x -> x > pivot) numbers  in
  (pivot, before_pivot, after_pivot)

let rec sort (numbers : int list) =
  match numbers with
  |hd :: [] -> [hd]
  |[] -> []
  |_ -> let (pivot,first_partition,second_partition) = partition numbers in
  sort first_partition @ [pivot] @ sort second_partition
