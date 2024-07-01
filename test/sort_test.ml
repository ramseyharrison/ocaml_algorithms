open Algorithms

let unsorted_list mtd () =
  Alcotest.(check(list int)) "same lists"
    (mtd [10;5;1;8])
    [1;5;8;10]
let empty_list mtd () = 
  Alcotest.(check(list int)) "same lists"
    (mtd [])
    []
let singleton_list mtd () = 
  Alcotest.(check(list int)) "same lists"
    (mtd [1])
    [1]

let () =
  (*Alcotest.run "Dummy" [ "Greeting", suite ]*)
  Alcotest.run "Algorithms"
    [
      ("Unsorted List",
       [Alcotest.test_case "Merge" `Quick (unsorted_list Merge_sort.sort);
        Alcotest.test_case "Quick" `Quick (unsorted_list Quick_sort.sort);
        Alcotest.test_case "Insertion" `Quick (unsorted_list Insertion_sort.sort); 
        Alcotest.test_case "Bubble" `Quick (unsorted_list Bubble_sort.sort); 
        Alcotest.test_case "Gnome" `Quick (unsorted_list Gnome_sort.sort); 
       ];);

       ("Empty List",
       [Alcotest.test_case "Merge" `Quick (empty_list Merge_sort.sort);
        Alcotest.test_case "Quick" `Quick (empty_list Quick_sort.sort);
        Alcotest.test_case "Insertion" `Quick (empty_list Insertion_sort.sort); 
        Alcotest.test_case "Bubble" `Quick (empty_list Bubble_sort.sort); 
        Alcotest.test_case "Gnome" `Quick (empty_list Gnome_sort.sort); 
       ];);

       ("Singleton List",
       [Alcotest.test_case "Merge" `Quick (singleton_list Merge_sort.sort);
        Alcotest.test_case "Quick" `Quick (singleton_list Quick_sort.sort);
        Alcotest.test_case "Insertion" `Quick (singleton_list Insertion_sort.sort); 
        Alcotest.test_case "Bubble" `Quick (singleton_list Bubble_sort.sort); 
        Alcotest.test_case "Gnome" `Quick (singleton_list Gnome_sort.sort); 
       ];)
    
    ]
