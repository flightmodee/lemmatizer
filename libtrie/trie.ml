open Base



type 'a t = Node of 'a list * ('a arc list) and 'a arc = char * 'a t

let empty inf = Node(inf, []);;


let rec string_to_word_aux str length = match length with
	| 0 -> []
	| _ -> let char = String.get str (length-1) in char::(string_to_word_aux str (length-1))

let string_to_word str = List.rev (string_to_word_aux str (String.length str))

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



let rec find (Node(info, arclist)) (wrd: char list) = match arclist with
	| [] -> let l = List.length wrd in if (l > 0) then [] else info
	| (chr1,Node(info2, arclist2))::bfrq -> (match wrd with
							| [] -> info
							| e::l -> if (Char.equal chr1 e) 
									  then (let nd = Node(info2, arclist2) in (find nd l)) 
									  else (find(Node(info, bfrq)) wrd))


let mem trie wrd = let l = List.length (find trie wrd) in if (l = 0) then false else true


let rec extract_aux (Node(info, arclist)) wrd_acc = match arclist with
	| [] -> [(List.rev wrd_acc, info)]
	| (chr1, Node(info2, arclist2))::bfrq -> let ndg = Node(info2, arclist2) in let ndd = Node(info, bfrq) in
	
										(match info with
											| [] -> if (List.is_empty bfrq) 
													then ((extract_aux ndg (chr1::wrd_acc))) 
													else ((extract_aux ndg (chr1::wrd_acc))@(extract_aux ndd wrd_acc)) 

											| _ ->  if (List.is_empty bfrq)
													then ((extract_aux ndg (chr1::wrd_acc)))
													else ((List.rev wrd_acc, info)::(extract_aux ndg (chr1::wrd_acc))@(extract_aux ndd wrd_acc)))


let extract (Node(info, arclist)) = extract_aux (Node(info, arclist)) []





