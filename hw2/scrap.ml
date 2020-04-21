(* 
    #use "hw2.ml";; 

    #use "hw2test.ml";;

    #use "scrap.ml";;

*)

type awksub_nonterminals_0 =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules_0 =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar_0 = Num, awksub_rules_0;;
let (_, production_function) = (convert_grammar awksub_grammar_0);;
let convert_grammar_test0 = (production_function Expr) = 
  [[T "("; N Expr; T ")"]; [N Num]; [N Expr; N Binop; N Expr]; [N Lvalue];[N Incrop; N Lvalue]; [N Lvalue; N Incrop]];;

let parse_tree_test0 =
  (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
   = [3; 4; 5]);;
let parse_tree_test1 =
  (parse_tree_leaves (Node ("+", [Node ("*", [Leaf 4; Leaf 5]); Leaf 3]))
  = [4;5;3]);;
let parse_tree_test2 = 
  (parse_tree_leaves (Node ("+", [Node ("*", [Node ("-", [Leaf 1; Leaf 2; Leaf 4]); Leaf 5]); Leaf 3]))
  = [1; 2; 4; 5; 3]);;
let parse_tree_test3 = 
  (parse_tree_leaves (Node ("+", [Node ("*", [Node ("-", [Leaf 1; Leaf 2; Node ("-", [Leaf 7; Leaf 8])]); Leaf 5]); Leaf 3]))
  = [1; 2; 7; 8; 5; 3]);;

(* question 3 aka the bane of my existence *)

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test0 =
  (make_matcher awkish_grammar accept_all ["ouch"]) = None

let test1 =
  (make_matcher awkish_grammar accept_all ["9"])
   = Some []

let test2 =
  (make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some ["+"]

let test3 =
  (make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None

let test4 =
 ((make_matcher awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some [])

let test5 =
  (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
   = [3; 4; 5])

let small_awk_frag = ["$"; "1"; "++"; "-"; "2"]

let test6 =
  ((make_parser awkish_grammar small_awk_frag)
   = Some (Node (Expr,
		 [Node (Term, [Node (Lvalue, [Leaf "$"; Node (Expr,[Node (Term, [Node (Num, [Leaf "1"])])])]);
			            Node (Incrop, [Leaf "++"])]
            );
		  Node (Binop, [Leaf "-"]);
		  Node (Expr, [Node (Term, [Node (Num, [Leaf "2"])])])
      ])))
let test7 =
  match make_parser awkish_grammar small_awk_frag with
    | Some tree -> parse_tree_leaves tree = small_awk_frag
    | _ -> false ;;