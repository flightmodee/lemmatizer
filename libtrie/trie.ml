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
	| (char,trie)::bfrq -> 1 + arc_size trie + arc_size(Node ([], bfrq))




let rec find (Node(info, arclist)) (wrd: char list) = match arclist with
	| [] -> let l = List.length wrd in if (l > 0) then [] else info
	| (chr1,Node(info2, arclist2))::bfrq -> (match wrd with
							| [] -> info
							| e::l -> if (Char.equal chr1 e) 
									  then (let nd = Node (info2, arclist2) in (find nd l)) 
									  else (find(Node(info, bfrq)) wrd))



let mem trie wrd = let l = List.length (find trie wrd) in if (l = 0) then false else true




let rec extract_aux (Node(info, arclist)) wrd_acc = match arclist with
	| [] ->  [(List.rev wrd_acc, info)] 
	| (chr1, Node(info2, arclist2))::bfrq -> (match info with
									| [] -> if (List.is_empty bfrq) 
											then ((extract_aux (Node(info2, arclist2)) (chr1::wrd_acc))) 
											else ((extract_aux (Node(info2, arclist2)) (chr1::wrd_acc))@(extract_aux (Node([], bfrq)) wrd_acc)) 

									| _ ->  if (List.is_empty bfrq)
											then ((List.rev wrd_acc, info)::extract_aux (Node (info2, arclist2)) (chr1::wrd_acc))
											else ((List.rev wrd_acc, info)::((extract_aux (Node (info2, arclist2)) (chr1::wrd_acc)))@(extract_aux (Node([], bfrq)) wrd_acc)))


	


let extract (Node(info, arclist)) = extract_aux (Node(info, arclist)) []


(* +++++++++++++++++++++++++++++++++++++++ Fonction sur les zippers ++++++++++++++++++++++++++++++++++++++++++ *)

(*Expliquons un peu les deux premiers types. L'idée principale est la suivante : lorsque l'on focus un noeud possédant plus d'un arc (donc à l'arclist comportant au moins deux éléments), et que l'on décide de descendre dans le premier fils dudit noeud, il faut conserver les arcs qui donnent vers les autres noeuds fils. Et inversement, il faut pouvoir, en remontant, rattacher une arclist à un noeud. Cependant, il faut pouvoir associer à chaque noeud son arclist. J'ai donc d'abord défini un type 'a chemin qui est concrètement le type du chemin d'un noeud à la racine, puis mon type final 'a path est constitué d'un couple comportant un chemin, ainsi qu'une liste d'arclist, me permettant de stocker, pour chaque noeud donné, son arclist correspondante. Un noeud ne possédant pas d'arcs externes possède une arclist vide, donc [].*)


type 'a chemin = Top | Chemin of 'a list * char * 'a chemin
type 'a path =  'a chemin * ('a arc list) list

type 'a zipper = Zipper of 'a t * 'a path


let trie_to_zipper trie = Zipper(trie, (Top, []))


(*let zip_up_exn (Zipper(Node(info, arclist), path)) = match path with
	| [] -> failwith "Up of first"
	| ([], _)::_ -> failwith "Up of first"
	| ((chr, Node(info_up, arclist_up))::_, bfrq_list)::_ -> match bfrq_list with
																| [] -> Zipper(Node(info_up, [(chr, Node(info, arclist))]), [(arclist_up, bfrq_list)])
																| arc::arcl -> Zipper(Node(info_up, [(chr, Node(info, arclist))]@[arc]), [(arclist_up, arcl)])*)

let zip_down_exn (Zipper (Node(info, arclist), (path, alist_l))) = match arclist with
	| [] -> failwith "Down of last"
	| (chr, Node(info_down, arclist_down))::bfrq -> match path with
													| Top -> Zipper(Node(info_down, arclist_down), (Chemin(info_down, chr, Top), (bfrq::alist_l)))
													| Chemin(info_p, char_p, chemin) -> Zipper(Node(info_down, arclist_down), (Chemin(info_down, chr, Chemin(info_p, char_p, chemin)), (bfrq::alist_l)))

let zip_up_exn (Zipper(Node(info, arclist), (path, alist_l))) = match path with
	| Top -> failwith "Up of first!"
	| Chemin (_, chr_n1, pth) -> match pth with
											| Top -> if (List.is_empty alist_l) 
													 then (Zipper(Node([], [(chr_n1, Node(info, arclist))]), (Top, alist_l))) 
													 else (let e = List.hd_exn alist_l in Zipper(Node([], [(chr_n1, Node(info, arclist))]@e), (Top, List.tl_exn alist_l)))
					
											
											
											| Chemin (info_n2, chr_n2, pth2) -> if (List.is_empty alist_l) 
																			 then (Zipper(Node(info_n2, [(chr_n1, Node(info, arclist))]), (Chemin(info_n2, chr_n2, pth2), alist_l)))
																			 else (let e = List.hd_exn alist_l in Zipper(Node(info_n2, [(chr_n1, Node(info, arclist))]@e), (Chemin(info_n2, chr_n2, pth2), List.tl_exn alist_l)))
