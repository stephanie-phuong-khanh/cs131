let subset_test0 = subset [1;2;3] [1;2;3]
let subset_test1 = subset [] []
let subset_test2 = subset [2] [1;2;3]
let subset_test3 = not (subset [4;5;6] [])
let subset_test4 = subset [3;5] [1;2;3;4;5]

let equal_sets_test0 = equal_sets [1;2;3;1] [3;2;1]
let equal_sets_test1 = not (equal_sets [1;3] [3;2])

let set_diff_test0 = equal_sets (set_diff [1;2;3;4] [1;2]) [3;4]
let set_diff_test1 = equal_sets (set_diff [3;2;1;1] [1;3]) [2]
let set_diff_test2 = equal_sets (set_diff [] [1;2;3]) []

let set_union_test0 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [] [4;5;6]) [4;5;6]
let set_union_test2 = equal_sets (set_union [4;5;6] []) [4;5;6]

let set_intersection_test0 = equal_sets (set_intersection [1;2;3;6;7] [1;2;3;4;5]) [1;2;3]
let set_intersection_test1 = equal_sets (set_intersection [] [1;2;3;4;5]) []

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x *. 3.) 2. = infinity
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x / 2) 500 = 0

type color_nonterminals =
  | Red | Blue | Yellow | Green | Orange | Black

let color_grammar =
  Red,
  [Green, [T"leaf"];
   Black, [];
   Yellow, [T"banana"];
   Orange, [T"pumpkin"];
   Blue, [N Black];
   Blue, [N Yellow];
   Blue, [N Orange];
   Red, [N Green];
   Red, [N Blue; T","; N Red]]

let color_test0 =
  filter_reachable color_grammar = color_grammar

let color_test1 =
  filter_reachable (Blue, List.tl (snd color_grammar)) =
    (Blue,
     [Black, []; Yellow, [T "banana"]; Orange, [T "pumpkin"];
      Blue, [N Black]; Blue, [N Yellow]; Blue, [N Orange]])
