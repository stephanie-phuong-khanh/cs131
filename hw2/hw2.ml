type ('nonterminal, 'terminal) symbol = 
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(* QUESTION 1 *)
let rec production_generator nonterminal rules = 
    match rules with
        | [] -> []
        | head::tail ->
            if (fst head) = nonterminal
            then (snd head)::(production_generator nonterminal tail)
            else (production_generator nonterminal tail)

let convert_grammar gram1 = 
    match gram1 with (this_nonterminal, rules) ->
    (this_nonterminal, (function nonterminal -> (production_generator nonterminal rules)))


(* QUESTION 2 *)
let rec leaf_nodes tree leaves =
  match tree with
    | Leaf leaf -> leaves@[leaf]
    | Node (node, list) -> 
      match list with
        | [] -> leaves
        | _ ->
          let returned_leaves = (leaf_nodes (List.hd list) leaves) in
          (leaf_nodes (Node (node, (List.tl list))) returned_leaves)

let parse_tree_leaves tree =
  leaf_nodes tree [];;

(* QUESTION 3 *)
let make_matcher_helper grammar start_symbol accept frag =
  let rec choose_rhs rhs_list acceptor frag = match rhs_list with
    | [] -> None
    | rhs_head::rhs_tail -> let result= (match_rule rhs_head acceptor frag) in
      if result = None then (choose_rhs rhs_tail acceptor frag) else result
  and match_rule rules acceptor frag = match rules with
    | [] -> (acceptor frag)
    | rules_head::rules_tail -> match frag with
      | [] -> None
      | frag_head::frag_tail -> match rules_head with
        | N rules_head -> choose_rhs (grammar rules_head) (match_rule rules_tail acceptor) frag
        | T rules_head -> if frag_head = rules_head then (match_rule rules_tail acceptor frag_tail) else None
  in (choose_rhs (grammar start_symbol) accept frag)
    
let rec make_matcher gram =
  match gram with (start_symbol,grammar) -> 
  (fun accept frag -> make_matcher_helper grammar start_symbol accept frag) ;;

(* QUESTION 4 *)
let accept_empty_suffix = function | [] -> Some [] | _ -> None ;;

let build_ruleset_dfs grammar start_symbol accept frag =
  let rec choose_rhs rhs_list acceptor frag = match rhs_list with
    | [] -> None
    | rhs_head::rhs_tail -> let result = (match_rule rhs_head acceptor frag) in
      match result with
      | None -> (choose_rhs rhs_tail acceptor frag)
      | Some rule_set -> Some (rhs_head::rule_set)
  and match_rule rules acceptor frag = match rules with
    | [] -> (acceptor frag)
    | rules_head::rules_tail -> match frag with
      | [] -> None
      | frag_head::frag_tail -> match rules_head with
        | N rules_head -> choose_rhs (grammar rules_head) (match_rule rules_tail acceptor) frag
        | T rules_head -> if frag_head = rules_head then (match_rule rules_tail acceptor frag_tail) else None
  in (choose_rhs (grammar start_symbol) accept frag)

let build_parse_tree ruleset start_symbol =
  let rec across ruleset current_rule = match current_rule with
    | [] -> ([], ruleset)
    | current_rule_head::current_rule_tail ->
      let (down_tree, down_remaining_ruleset) = (down ruleset current_rule_head) in
      let (across_tree, across_remaining_ruleset) = (across down_remaining_ruleset current_rule_tail) in
      (down_tree::across_tree, across_remaining_ruleset)
  and down ruleset current_symbol = match current_symbol with
    | T current_symbol -> (Leaf current_symbol, ruleset)
    | N current_symbol ->
      match ruleset with ruleset_head::ruleset_tail ->
      let (tree,remaining_ruleset) = (across ruleset_tail ruleset_head) in
      (Node (current_symbol, tree), remaining_ruleset)
  in let (tree,_) = (down ruleset (N start_symbol)) in Some tree

let make_parser_helper gram frag =
  match gram with (start_symbol, grammar) ->
    match (build_ruleset_dfs grammar start_symbol accept_empty_suffix frag) with
    | None -> None
    | Some ruleset -> (build_parse_tree ruleset start_symbol)

let make_parser gram =
  fun frag -> (make_parser_helper gram frag) ;;