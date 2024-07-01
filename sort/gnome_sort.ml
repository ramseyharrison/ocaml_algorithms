let sort (numbers : int list) =
  let rec loop (numbers : int list) (sorted : int list) =
    match numbers with
    | first :: second :: tl -> (
        if first <= second then loop (second :: tl) (first :: sorted)
        else
          match sorted with
          | hd :: tl_rev -> loop (hd :: second :: first :: tl) tl_rev
          | _ -> loop (second :: first :: tl) sorted)
    | tl :: [] -> List.rev (tl :: sorted)
    | [] -> []
  in
  loop numbers []
