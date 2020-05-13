open Base
open String



type 'a t = Node of 'a list * ('a arc list) and 'a arc = char * 'a t


let rec string_to_word_aux str length = match length with
	| 0 -> []
	| _ -> let char = get str (length-1) in char::(string_to_word_aux str (length-1))

let string_to_word str = List.rev (string_to_word_aux str (length str))

let rec word_to_string wrd = match wrd with
	| [] -> ""
	| e::res -> let chr = Char.escaped e in chr^(word_to_string res)




let empty inf = Node(inf, []);;





	
