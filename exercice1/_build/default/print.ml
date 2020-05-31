open Stdio
open Base

(* val extract: In_channel -> (string,string,string) list *)
(* lit chaque ligne du fichier (In_channel est équivalent du FILE* du langage C) *)
(* et crée une liste de triplets (forme,catégorie,lemme) *)
(* pour transformer le ic en string, vous utiliserez les fonctions input_line ou fold_lines *)
(* cf. la documentation https://ocaml.janestreet.com/ocaml-core/latest/doc/stdio/Stdio/In_channel/index.html *)
(* vous appellerez ensuite extract_line *)

(*La fonction Base.Option.get n'existant pas, j'ai redéfini une fonction à peu près similaire*)

let getOption v = match v with
	| Some e -> e
	| None -> failwith "option is none";;



(*Cette fonction est utilisée dans extract_line, et se charge basiquement de renvoyer le triplet lexème/catégorie/lemme que l'on cherche*)

let split_aux (sl:string list) : (string * string * string) = match sl with
	| a::_::c::_::e::_ -> (a,c, getOption (List.hd (String.split e ~on:'_')))
	| _ -> failwith "This shouldn't have happened. Stop. Get some help."
;;


(*La fonction extract_line prend une chaîne de caractères en arguments, et la fait passer dans la fonction split_aux qui se charge de fournir le triplet recherché.*)

let extract_line (line:string) : (string*string*string) = let l = String.split line ~on:'\t' in split_aux l;;


(*La fonction extract est ici définie à l'aide d'une sous-fonction récursive : elle prend un descripteur de fichier en paramètre, et fonctionne de la façon suivante.
Grâce à un let binding nous permettant de recueillir chaque ligne du fichier décrit par ic. Ainsi, notre variable line recevra une option. Il faut donc faire en sorte
que la fonction lise chaque ligne jusqu'à la fin, et il se trouve que lorsque l'on se trouve à la fin d'un fichier texte et que l'on essaie de le lire avec input_line, cela renvoie Base.Option.None. Notre condition d'arrêt est donc trouvée, et la fonction renvoie la liste formée de tous les triplets de notre fichier décrit par ic.
Petit inconvénient de cette fonction, il faut que le fichier ne comporte pas de ligne vide, ni de lignes consécutives séparées par une ligne vide. Mais dans le cas du fichier fourni dans le projet, ce n'est pas le cas.*)

let rec extract_rec ic  = let line = Stdio.In_channel.input_line ic in match line with
	| Some _ -> let triplet = getOption line |> extract_line in triplet::(extract_rec ic)
	| None -> []
;;

let extract ic = extract_rec ic;;


let () =
  In_channel.create (Sys.get_argv()).(1)
  |> extract 
  |> List.iter ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l);;




