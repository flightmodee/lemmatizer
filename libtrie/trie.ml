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
	| [] ->  [(List.rev wrd_acc, info)] 
	| (chr1, Node(info2, arclist2))::bfrq -> (match info with
									| [] -> if (List.is_empty bfrq) 
											then ((extract_aux (Node(info2, arclist2)) (chr1::wrd_acc))) 
											else ((extract_aux (Node(info2, arclist2)) (chr1::wrd_acc))@(extract_aux (Node([], bfrq)) wrd_acc)) 

									| _ ->  if (List.is_empty bfrq)
											then ((List.rev wrd_acc, info)::extract_aux (Node(info2, arclist2)) (chr1::wrd_acc))
											else ((List.rev wrd_acc, info)::((extract_aux (Node(info2, arclist2)) (chr1::wrd_acc)))@(extract_aux (Node([], bfrq)) wrd_acc)))


	


let extract (Node(info, arclist)) = extract_aux (Node(info, arclist)) []


(* +++++++++++++++++++++++++++++++++++++++ Fonction sur les zippers ++++++++++++++++++++++++++++++++++++++++++ *)

(*Mon type 'a path est défini ainsi : il s'agit d'une liste de couple d'arcs. L'arc list de gauche correspond au chemin qui lie le noeud focusé à la racine (qu'on implémente ici sous forme d'une arclist qui ne contiendra qu'un seul élément, pour que l'on puisse travailler avec dans le constructeur Node)
Quant à l'autre arclist, elle  me permet de stocker une liste d'arcs lorsque l'on se trouve dans un noeud à plusieurs bifurcations et que l'on descend : ainsi, en descendant, on conserve l'arclist du noeud précédent, ce qui nous servira à reconstituer le tout en remontant.*)

type 'a path = ('a arc list * 'a arc list) list

type 'a zipper = Zipper of 'a t * 'a path


let trie_to_zipper trie = Zipper(trie,[])

let zip_down_exn (Zipper(Node(info, arclist), path)) = match arclist with
	| [] -> failwith "Down of last"
	| (chr, Node(info_down, arclist_down))::bfrq -> match path with 
												| [] -> Zipper(Node(info_down, arclist_down), [([(chr, (Node(info, [])))], bfrq)])
												| (pth, bfrq_list)::_ -> Zipper(Node(info_down, arclist_down), [([(chr, (Node(info, pth)))], bfrq@bfrq_list)])


let zip_up_exn (Zipper(Node(info, arclist), path)) = match path with
	| [] -> failwith "Up of first"
	| ([(chr, Node(info_up, arclist_up))], bfrq_list)::_ -> match bfrq_list with
																| [] -> Zipper(Node(info_up, [(chr, Node(info, arclist))]), [(arclist_up, bfrq_list)])
																| arc::arcl -> Zipper(Node(info_up, [(chr, Node(info, arclist))]@[arc]), [(arclist_up, arcl)])




