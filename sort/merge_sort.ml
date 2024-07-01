let split_point (index : int) (list : int list) =
  let rec loop (state : (int * int list * int list)) (list : int list) = 
    match list with
    |[] -> ([],[])
    |hd::tl ->
      match state with
      |(0,left,[]) -> (List.rev left,list)
      |(x,left,[]) -> loop (x - 1, hd::left, []) tl
      |_ -> ([],[])
  in loop (index,[],[]) list


let rec merge (left : int list) (right : int list) =
  match left, right with
  |[], [] -> []
  |[], _ -> right
  |_, [] -> left
  |hdl :: tll, hdr :: tlr ->
    if hdl < hdr then hdl :: merge tll right
    else if hdr < hdl then hdr :: merge left tlr
    else hdl :: hdr :: (merge tll tlr)
                         
let rec sort (list : int list) =
  match list with
  |hd :: [] -> [hd]
  |_ :: _ ->
    let marker = (List.length list) / 2 in
    let (left,right) = split_point marker list in
    let sort_left = sort left in
    let sort_right = sort right in
    merge sort_left sort_right
  |[] -> []
