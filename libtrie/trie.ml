open Base



type 'a t = Node of 'a list * ('a arc list) and 'a arc = char * 'a t

let empty inf = Node(inf, [])


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

(*Voici le type récursif que j'ai choisi d'implémenter pour représenter le chemin d'un focus à la racine. Le type path est doté de deux constructeurs : un Top ne prenant aucun argument, signifiant que nous nous trouvons à la racine de notre trie, et un constructeur NoeudP qui décrit le contexte d'un sous-trie, celui-ci est un peu particulier. J'ai choisi de considérer des arcs en tant que contextes gauche et droit d'un noeud focus, et non pas des tries entiers, et ce par pur souci de lisibilité, car cela aurait été lourd à lire, comprendre. Ainsi, ce constructeur prend en entrée une liste inversée d'arcs gauches (permettant de directement travailler avec les têtes de liste), un char qui représente le symbole consommé par l'arc externe du père du noeud focus à ce dernier, une 'a list représentant l'information de ce noeud, un 'a path (pour faire avancer la récursion) et enfin une liste d'arcs à droite. Concrètement, j'ai décidé de considérer des arcs afin de pouvoir garder en mémoire l'information du char permettant d'arriver à un noeud, là où avec de simples tries, on l'aurait perdue.

Là où dans les arbres n-aires, les noeuds ne sont que de simples liens structurels sans aucune information, les noeuds des tries en détiennent une (vide ou non), et qui plus est, les arcs sont également des structures dotées d'une information (le char qui permet de passer dans ladite transition), il fallait donc prendre tout cela en compte.

Si j'avais décidé de considérer des listes de 'a t au lieu d'a arcs, cela aurait été lourd et peu visible (nous aurions eu NoeudP(Node etc.....), ce qui, à mon sens, facilite peu la compréhension. Mon implémentation semble plus adaptée et m'est plus lisible.*)



type 'a path = Top | NoeudP of 'a arc list * char * 'a list * 'a path * 'a arc list


(*On notera par ailleurs que j'ai pris la liberté de changer de place les deux champs du constructeur, dans un souci de lisibilité.*)
type 'a zipper = Zipper of 'a t * 'a path


(*Fonction de base, qui crée un zipper à partir d'un trie, en plaçant le focus à la racine du trie.*)
let trie_to_zipper trie = Zipper(trie, Top)


(*Cette fonction prend un Zipper en argument, déplace le focus vers le premier noeud fils, si possible, et renvoie le zipper correspondant.*)
let zip_down_exn (Zipper (t, p)) = match t with
	| Node (_, []) -> failwith "Down of last"
	| Node (info, (chr,t)::arcl) -> if (List.is_empty arcl) 
									then (Zipper (t, NoeudP ([], chr, info, p, []))) 
									else (Zipper (t, NoeudP ([], chr, info, p, arcl)))


(*Cette fonction prend un Zipper en argument, déplace le focus sur le noeud père, si possible, puis renvoie le zipper correspondant.*)
let zip_up_exn (Zipper (t, p)) = match p with
	| Top -> failwith "Up of first" 
	| NoeudP (l, chr, info, p, r) -> Zipper (Node (info, List.rev_append l ((chr,t)::r)), p)


(*Cette fonction prend un Zipper en argument, déplace le focus sur le frère gauche précédent, si possible, et renvoie le zipper correspondant.*)
let zip_left_exn (Zipper (t, p)) = match p with
	| Top -> failwith "Left of Top"
	| NoeudP ([], _, _, _, _) -> failwith "No left trie"
	| NoeudP ((chr_l, n)::left, chr, info, p, right) -> Zipper (n, NoeudP (left, chr_l, info, p, ((chr, t)::right)))


(*Cette fonction prend un Zipper en argument, déplace le focus sur le frère gauche précédent, si possible, et renvoie le zipper correspondant.*)
let zip_right_exn (Zipper (t, p)) = match p with
	| Top -> failwith "right of Top"
	| NoeudP (_, _, _, _, []) -> failwith "right of last"
	| NoeudP (left, chr, info, p, (chr_r, n)::right) -> Zipper (n, NoeudP ((chr, t)::left, chr_r, info, p, right))


(*Les quatre fonctions qui suivent sont assez équivoques, de par leur nom.*)

let rec zip_up_until pred z = if (pred z) then z else zip_up_exn z |> zip_up_until pred

let rec zip_down_until pred z = if (pred z) then z else zip_down_exn z |> zip_down_until pred

let rec zip_left_until pred z = if (pred z) then z else zip_left_exn z |> zip_left_until pred

let rec zip_right_until pred z = if (pred z) then z else zip_right_exn z |> zip_right_until pred


(*Cette fonction permet de renvoyer le trie correspondant au zipper passé en paramètre.*)
let rec zipper_to_trie (Zipper (Node (info, arclist), path)) = match path with
	| Top -> Node (info, arclist)
	| NoeudP (l, chr, info_n, p, r) -> zipper_to_trie (Zipper (Node (info_n, List.rev_append l ((chr, Node (info, arclist))::r)), p))



(*Petite précision d'implémentation concernant les deux fonctions qui vont suivre : le commentaire du fichier .mli 
ne donnait que peu de précisions quant aux insertions, j'ai donc pris le parti, pour chacune de ces deux fonctions
prenant un Zipper ainsi qu'un char et un 'a t en argument (les deux derniers arguments formeront donc l'arc à insérer)
d'insérer l'arc en question au niveau du père du noeud focus, et non pas dans le focus, cela me semblant plus naturel. Ainsi, dans la structure 'a path, pour inserer_right, par exemple, la liste droite sera modifiée et acceptera un nouvel arc formé par les arguments de la fonction.*)

let zip_insert_right (Zipper (t, p)) (c: char) node = match p with
	| Top -> failwith "Insert of top"
	| NoeudP (l, chr, info, u, r) -> Zipper (t, NoeudP (l, chr, info, u, (c,node)::r))


let zip_insert_left (Zipper (t, p)) (c: char) node = match p with
	| Top -> failwith "Insert of Top"
	| NoeudP (l, chr, info, u, r) -> Zipper (t, NoeudP ((c,node)::l, chr, info, u, r))




(*J'ai utilisé une fonction annexe dans ma fonction insert : en effet, je me suis heurté à un problème d'implémentation. Lorsque je me trouvais 
dans un Node comportant plusieurs arcs, il me fallait effectuer une récursion sur chaque arc. 
Par exemple, si le char du premier arc ne correspondait pas avec la tête du mot passé en argument, 
on passait à l'autre arc. Or, cela n'était pas possible avec la signature
de la fonction insert, qui imposait de travailler avec tout le noeud, il me fallait trouver
un moyen de pouvoir travailler de façon individuelle avec l'arclist de chaque noeud.
J'ai donc décidé d'extraire la liste d'arcs, 
et d'utiliser un accumulateur qui me permet de récupérer chaque arc, comme vous pourrez le voir dans le code ci-dessous.*)



let rec insert_aux (Node (i, a)) arclist w d = match w with
	| [] -> Node ([d], a@arclist)
	| e::l -> match arclist with
				| [] -> Node (i, a@[(e, word_to_trie l [d])])
				| (chr, Node (info, al))::reste -> if (Char.equal e chr)
									 then (let ins = insert_aux (Node (info, [])) al l d in 
											Node (i, (e, ins)::reste))
									 else (insert_aux (Node (i, a@[(chr, Node(info, al))])) reste (e::l) d)


let insert (Node (info, alist)) w d = insert_aux (Node (info, [])) alist w d
