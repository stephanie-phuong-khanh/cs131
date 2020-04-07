type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal

(*
*   returns only elements of a that are also elements in b
*)
let rec subset a b =
    match a with
    | [] -> true
    | a_head::a_tail ->
        if List.mem a_head b
        then subset a_tail b
        else false

(*
*   two sets are equal if one set is the subset of the other set and vice versa
*)
let equal_sets a b = 
    (subset a b) && (subset b a)

(*
*   recursively goes through every element of a and returns only those belonging to only a, not b
*)
let rec set_diff a b = 
    match a with
    | [] -> []
    | a_head::a_tail ->
        if List.mem a_head b
        then set_diff a_tail b
        else a_head::(set_diff a_tail b)

(*
*   values belonging to only set a + values belonging to only set b + valeus belonging to both
*)
let set_union a b = 
    a @ (set_diff b a)

(*
*   intersection of sets a and b is equal to their set union minus (a-b) and (b-a)
*)
let set_intersection a b =
    (set_diff (set_diff (set_union a b) (set_diff a b)) (set_diff b a))

(*
*   computed_fixed_point applies function f to x, using that result as x in the next recursive call until f(x)=x
*)
let rec computed_fixed_point eq f x =
    let f_of_x = (f x) in
        if (eq f_of_x x)
        then x
        else (computed_fixed_point eq f f_of_x)

(*
*   helper function to find_reachable. similar to computed_fixed_points but takes reachable_symbols and
*   rules as separate arguments because every recursive call must be passed the original rules list, since
*   the rules list returned from find_reachable_rules is always empty and cannot be used for the next call.
*)
let rec grammar_cfp eq f reachable_symbols rules =
    let x = (reachable_symbols, rules) in
    let result = (f x) in
        if (eq result x)
        then x
        else (grammar_cfp eq f (fst result) rules)

(*
*   helper function to find_reachable. passed as argument to grammar_cfp to compare the first
*   element in the argument tuple, which here is the list of reachable symbols.
*)
let equal_first_el x y =
    equal_sets (fst x) (fst y)

(*
*   helper function to find_reachable_rules. returns true if argument is of type N.
*)
let check_N_type x =
    match x with
        | N x -> true
        | _ -> false

(*
*   helper function to find_reachable symbols. returns reachable symbol of type nonterminal.
*)
let extract x =
    match x with
        | N x -> x
        | _ -> failwith "error: nonterminal is not of type N"

(*
*   helper function to filter_reachable function. passed to grammar_cfp as argument.
*   works by recursively iterating through the list of rules and appending to list of reachable symbols
*   all the nonterminal symbols reached in this step
*)
let rec find_reachable_rules params =
    match params with (reachable_symbols, rules) ->
    match rules with
    | [] -> (reachable_symbols, rules)
    | first_rule::rest_of_rules ->
        match first_rule with (first_rule_left,first_rule_right) -> 
            if List.mem first_rule_left reachable_symbols
            then 
                let nonterminals = List.filter(fun x -> check_N_type x) first_rule_right in
                let new_reachable_symbols = set_union reachable_symbols (List.map(fun x -> extract x) nonterminals) in
                find_reachable_rules (new_reachable_symbols,rest_of_rules)
            else
                find_reachable_rules (reachable_symbols,rest_of_rules)

(*
*   returns copy of grammar with only reachable rules, in same order as provided in g
*)
let filter_reachable g =
    match g with (start_symbol,rules) ->
        (* Find reachable_rules by using grammar_cfp to continually explore set of rules and append 
        to reachable_rules list until reachable_rules remains the same after applying find_reachable_rules *)
        let (reachable_symbols,_) = grammar_cfp equal_first_el find_reachable_rules [start_symbol] rules in
        let filtered_rules = List.filter(fun x -> (List.mem (fst x) reachable_symbols)) rules in
        start_symbol, filtered_rules