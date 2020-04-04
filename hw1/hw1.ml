type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal

let rec subset a b =
    match a with
    | [] -> true
    | a_head::a_tail ->
        if List.mem a_head b
        then subset a_tail b
        else false

let equal_sets a b = 
    (subset a b) && (subset b a)

let rec set_diff a b = 
    match a with
    | [] -> []
    | a_head::a_tail ->
        if List.mem a_head b
        then set_diff a_tail b
        else a_head::(set_diff a_tail b)

let set_union a b = 
    a @ (set_diff b a)

let set_intersection a b =
    (set_diff (set_diff (set_union a b) (set_diff a b)) (set_diff b a))

exception Foo of string

let rec computed_fixed_point eq f x =
    let f_of_x = (f x) in
        if (eq f_of_x x)
        then x
        else (computed_fixed_point eq f f_of_x)

(* OK, now for the real work. Write a function filter_reachable g that returns a copy of the grammar g
with all unreachable rules removed. This function should preserve the order of rules: that is, all
rules that are returned should be in the same order as the rules in g. *)
let rec grammar_cfp eq f reachable_symbols rules =
    let x = (reachable_symbols, rules) in
    let result = (f x) in
        if (eq result x)
        then x
        else (grammar_cfp eq f (fst result) rules)

let equal_first_el x y =
    equal_sets (fst x) (fst y)

let check_N_type x =
    match x with
        | N x -> true
        | _ -> false

let extract x =
    match x with
        | N x -> x

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

let filter_reachable g =
    match g with (start_symbol,rules) ->
        (* Find reachable_rules by using grammar_cfp to continually explore set of rules and append 
        to reachable_rules list until reachable_rules remains the same after applying find_reachable_rules *)
        let (reachable_symbols,_) = grammar_cfp equal_first_el find_reachable_rules [start_symbol] rules in
        let filtered_rules = List.filter(fun x -> (List.mem (fst x) reachable_symbols)) rules in
        start_symbol, filtered_rules