type word = char list
(* "bonjour" -> ['b';'o';'n';'j';'o';'u';'r'] *)
val string_to_word : string -> word
(* ['b';'o';'n';'j';'o';'u';'r'] -> "bonjour" *)
val word_to_string : word -> string


(* type *)
type 'a t [@@deriving show]


(* crée un trie avec la chaîne vide comme préfixe *)
val empty : 'a list -> 'a t

(* crée un trie qui contient le premier argument avec les infos du 2nd argument *)
val word_to_trie : word -> 'a list -> 'a t

(* renvoie le nombre de mots stockés dans le trie *)
val size : 'a t -> int

(* renvoie le nombre d'arcs (et donc de caractères) stockés dans le trie *)
val arc_size : 'a t -> int

(* renvoie la liste d'informations associée à un mot par le trie *)
(* renvoie la liste vide si le mot n'appartient pas au trie *)
val find : 'a t -> word -> 'a list

(* renvoie vrai si le mot est dans le trie, faux sinon *)
val mem : 'a t -> word -> bool

(* renvoie la liste de mots contenus associés à leurs informations *)
val extract : 'a t -> (word * 'a list) list

type 'a path
type 'a zipper = Zipper of 'a path * 'a t

exception Up
exception Down
exception Left
exception Right

(* renvoie un zipper avec focus sur la racine de l'arbre *)
val trie_to_zipper : 'a t -> 'a zipper

(* déplace le focus vers le noeud pere *)
(* lève l'exception Up si le focus est la racine de l'arbre *)
val zip_up_exn   : 'a zipper -> 'a zipper

(* déplace le focus vers le premier noeud fils *)
(* lève l'exception Down si le focus n'a pas de noeud fils *)
val zip_down_exn : 'a zipper -> 'a zipper

(* déplace le focus vers le frere gauche précédent *)
(* lève l'exception Left si le focus n'a pas de frere gauche *)
val zip_left_exn : 'a zipper -> 'a zipper

(* déplace le focus vers le frere droit précédent *)
(* lève l'exception Right si le focus n'a pas de frere droit *)
val zip_right_exn : 'a zipper -> 'a zipper

(* zip_up_until p z déplace le focus vers le haut jusqu'à ce que le prédicat p soit vrai *)
val zip_up_until    : ('a zipper -> bool) -> 'a zipper -> 'a zipper

(* zip_down_until p z déplace le focus vers le bas jusqu'à ce que le prédicat p soit vrai *)
val zip_down_until  : ('a zipper -> bool) -> 'a zipper -> 'a zipper

(* zip_left_until p z déplace le focus vers la gauche jusqu'à ce que le prédicat p soit vrai *)
val zip_left_until  : ('a zipper -> bool) -> 'a zipper -> 'a zipper

(* zip_right_until p z déplace le focus vers la droite jusqu'à ce que le prédicat p soit vrai *)
val zip_right_until : ('a zipper -> bool) -> 'a zipper -> 'a zipper


(*renvoie le trie correspondant au zipper passé en paramètre *)
val zipper_to_trie : 'a zipper -> 'a t


(* insère à droite du focus *)
val zip_insert_right : 'a zipper -> char -> 'a t -> 'a zipper

(* insère à gauche du focus *)
val zip_insert_left  : 'a zipper -> char -> 'a t -> 'a zipper

(* insère dans un trie un mot associée à une nouvelle donnée, renvoie le le trie mis à jour*)
val insert : 'a t -> word -> 'a -> 'a t
