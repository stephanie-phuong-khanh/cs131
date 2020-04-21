let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type nonterminals =
  | Sentence | NounPhrase | VerbPhrase | Noun | Adjective | Verb | Adverb | Article

let grammar =
  (Sentence,
  function
    | Sentence ->
      [[N NounPhrase; N VerbPhrase];
      [N NounPhrase; N VerbPhrase; N NounPhrase]]
    | NounPhrase ->
      [[N Noun];
      [N Article; N Noun];
      [N Adjective; N Noun];
      [N Article; N Adjective; N Noun];
      [N Adverb; N Adjective; N Noun];
      [N Article; N Adverb; N Adjective; N Noun]]
    | VerbPhrase ->
      [[N Verb];
      [N Adverb; N Verb]]
    | Noun ->
      [[T "fish"]; [T "frog"]; [T "cat"]; [T "popcorn"]]
    | Adjective ->
      [[T "funny"]; [T "spicy"]; [T "blue"]; [T "furry"]; [T "happy"]]
    | Verb -> 
      [[T "eats"]; [T "likes"]; [T "cooks"]; [T "births"]]
    | Adverb -> 
      [[T "stupidly"]; [T "lovingly"]; [T "clumsily"]; [T "adeptly"]]
    | Article ->
      [[T "the"]; [T "a"]; [T "this"]; [T "that"]]
  )

let frag = ["this"; "blue"; "cat"; "clumsily"; "cooks"; "spicy"; "popcorn"]

let make_matcher_test = (make_matcher grammar accept_empty_suffix frag) = Some []

let make_parser_test = match (make_parser grammar frag) with
  | Some tree -> parse_tree_leaves tree = frag
  | _ -> false