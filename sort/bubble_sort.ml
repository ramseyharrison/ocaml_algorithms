let pass (numbers : int list) : bool * int list =
  let swap = ref false in
  let rec loop (numbers : int list) : int list =
    match numbers with
    | [] -> []
    | first :: second :: tl ->
        if first > second then (
          swap := true;
          second :: loop (first :: tl))
        else first :: loop (second :: tl)
    | elmt :: [] -> [ elmt ]
  in
  (!swap, loop numbers)

let sort (numbers : int list) =
  let rec loop (curr : int list) =
    let swap, update = pass curr in
    if swap then loop update else update
  in
  loop numbers
