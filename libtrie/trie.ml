open Base
open String



type 'a t = Node of 'a list * ('a arc list) and 'a arc = char * 'a t

let empty inf = Node(inf, []);;


let rec string_to_word_aux str length = match length with
	| 0 -> []
	| _ -> let char = get str (length-1) in char::(string_to_word_aux str (length-1))

let string_to_word str = List.rev (string_to_word_aux str (length str))

let rec word_to_string wrd = match wrd with
	| [] -> ""
	| e::res -> let chr = Char.escaped e in chr^(word_to_string res)


let rec word_to_trie wrd vals = match wrd with
	| [] -> Node(vals, [])
	| e::res -> Node([], [(e, (word_to_trie res vals))])



let rec size (Node(inf, arclist)) = match inf with
	| [] -> (match arclist with
			| [] -> 0
			| (char,trie)::bfrq -> size trie + size(Node([], bfrq)))
	
	| _  -> (match arclist with
			| [] -> 1
			| (char,trie)::bfrq -> 1 + size trie + size(Node([], bfrq)))


let rec arc_size (Node(inf, arclist)) = match arclist with
	| [] -> 0
	| (char,trie)::bfrq -> 1 + arc_size trie + arc_size(Node([], bfrq))



let rec find (Node(inf, arclist)) (wrd: char list) = match arclist with
	| [] -> inf
	| (chr1,Node(inf2, arclist2))::bfrq -> (match wrd with
							| [] -> inf
							| e::l -> if (Char.equal chr1 e) 
									  then (let nd = Node(inf2, arclist2) in (find nd l)) 
									  else (find(Node(inf, bfrq)) wrd))







	
