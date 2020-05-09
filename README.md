
# Table of Contents

1.  [Introduction](#org1269b96)
2.  [Récupérer le projet et procédure de rendu](#org9b67d7c)
3.  [Données](#org7e1bfe8)
    1.  [Lire les données (exercices 1 et 2)](#orgd7b5576)
4.  [Stocker les mots](#org1354858)
    1.  [Ajouter le lien entre formes, catégories et lemmes](#org2ac161d)



<a id="org1269b96"></a>

# Introduction

La lemmatisation désigne le traitement d&rsquo;un texte dans lequel on trouve pour
chaque mot (ou lexème, ou forme fléchie) son *lemme* (ou forme canonique).
Concrètement, en français pour les noms on renvoie la variante au singulier,pour
les adjectifs on renvoie la variante au masculin singulier, et pour les verbes on
renvoie l&rsquo;infinitif. Pour les autres types de mot (prépositions adverbes
etc&#x2026;), on les renvoie tels quels. Par exemple:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">forme</th>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-left">lemme</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">joyeuses</td>
<td class="org-left">&rarr;</td>
<td class="org-left">joyeux</td>
</tr>


<tr>
<td class="org-left">bonheurs</td>
<td class="org-left">&rarr;</td>
<td class="org-left">bonheur</td>
</tr>


<tr>
<td class="org-left">chanteraient</td>
<td class="org-left">&rarr;</td>
<td class="org-left">chanter</td>
</tr>


<tr>
<td class="org-left">gaîment</td>
<td class="org-left">&rarr;</td>
<td class="org-left">gaîment</td>
</tr>
</tbody>
</table>

Bien sûr c&rsquo;est parfois une opération ambiguë, c&rsquo;est-à-dire que pour certaines
formes plusieurs lemmes peuvent convenir. Par exemple `portes` peut être le
pluriel du nom `porte` ou une conjugaison tu verbe `porter`. Dans ce cas, la
lemmatisation doit renvoyer les deux lemmes. On remarque que dans ce cas si on
connaît la catégorie du mot (nom ou verbe), la lemmatisation n&rsquo;est plus ambiguë.

Le but de ce projet est de construire un lexique pour ensuite s&rsquo;en servir dans
un lemmatisateur, qui sera interrogeable à distance en client/serveur.

Le projet est à faire en binôme et à remettre pour le 25/5 (23h59) sous la forme
d&rsquo;une *pull request* sur github.


<a id="org9b67d7c"></a>

# Récupérer le projet et procédure de rendu

-   Sur l&rsquo;ENT du cours vous aurez accès l&rsquo;invitation pour le projet.
-   Il faudra indiquer les membres de votre binôme et le nom de votre *équipe*.
    Merci de l&rsquo;appeler `NOM1_NOM2` pour faciliter la correction. Avant de continuer,
    assurez-vous que les deux membres du binôme ont un compte sur github.
-   vous aurez ensuite accès à un dépot git personnel pour votre binôme. Vous
    devez le cloner pour le copier localement sur votre machine de travail (votre
    ordinateur personnel ou l&rsquo;ordinateur d&rsquo;une salle TP). La commande a la forme:
    
        mkdir -p $HOME/PRIPRO
        cd $HOME/PRIPRO
        git clone https://github.com/URLDEMOMDEPOTGIT.git  #mettre l'URL correcte
        git config --global user.email "monemail@paris13.fr" #mettre votre email
        git config --global user.name "prenom nom" #mettre vos nom et prenom
        git config --global push.default simple
    
    -   Ensuite, la première chose à faire est de créer une branche de travail pour
        pouvoir mettre à jour facilement l&rsquo;énoncé si besoin et pour pouvoir faire
        des pull requests (cf. infra). Vous ne pourrez pas rendre votre devoir si
        vouis ne le faites pas.
        On peut par exemple nommer cette branche `travail` avec la commande suivante:
        
            cd $HOME/PRIPRO/URLDEMONDEPOTGIT
            git branch #affiche la branche courante (master)
            git checkout -b travail #creation de la branche appelee travail
            git branch #affiche la branche courante (maintenant travail)
        
        **Vous devrez toujours travailler dans la branche de travail**, sinon vous ne
        pourrez pas rendre votre projet comme il faut (et vous aurez donc des points
        en moins)
    
    -   Pour ce projet, à la fin de chaque exercice, vous effectuez la liste de commandes suivante (évidemment à modifier selon besoin):
        
            git add fic1 fic2 #(fic1 et fic2 sont des fichiers modifiés ou créés pour l'exercice N)
            git commit -m "Exercice N"
            git push --set-upstream origin travail # la premiere fois, creer une branche travail sur le serveur en même temps que sauvegarde du code
            git push # les autres fois
    
    -   Quand vous avez fini le projet, créez une pull request à partir de la page
        github de votre dépôt. Il faut d&rsquo;abord choisir dans le menu Branch la
        branche travail, puis comparer la branche master et la branche travail


<a id="org7e1bfe8"></a>

# Données

Les données pour construire le lemmatiseur sont stockées dans le ficher
`extract-lefff-3.4.elex` que vous trouverez dans l&rsquo;archive. Sur chaque ligne on
trouve pour un mot (une forme fléchie) des règles morpho-syntaxiques sous la
forme de 9 champs séparés par des tabulations. Par exemple (en coupant la ligne
en deux pour faciliter la mise en page) :

    abonda 100 v [pred="abonder____1<Suj:cln|sn,Objde:de-sn|en>",@pers,cat=v,@J3s]
           abonder_1 ThirdSing J3s %actif v-er:std

Cette ligne indique les informations suivantes:

-   le mot (forme fléchie) est `abonda`;
-   la règle a une priorité de 100;
-   la catégorie du mot est `v` (pour verbe)
-   des informations sur la construction syntaxique entre crochets;
-   le lemme et sa variante, ici `abonder____1` pour indique la variante `1` du
    lemme `abonder`
-   puis des informations propres à la catégorie (ici `ThirdSing` pour 3e personne
    du singulier avec pour code `J3s`, `%actif` pour indiquer la voix, et
    `v-er:std` pour indiquer le type de la conjugaison du verbe, ici la
    conjugaison standard des verbes qui finissent par `-er`.


<a id="orgd7b5576"></a>

## Lire les données (exercices 1 et 2)

Dans cette première partie, il s&rsquo;agit d&rsquo;écrire un programme qui lit le fichier
de lexique (`lefff-3.4.elex`) et crée pour chaque ligne de règle un triplet
`(forme fléchie, catégorie, lemme)`; ces triplets sont stockés dans une liste
qui est affichée, un triplet par ligne.

1.  Écrire une fonction `extract_line` qui extrait d&rsquo;une ligne du fichier de
    lexique la forme fléchie, la catégorie et le lemme et qui sera utilisée dans
    les questions suivantes. Son prototype est le suivant:
    
        (* pour récupérer les informations dans la string, vous utiliserez la fonction split, cf. *)
        (* https://ocaml.janestreet.com/ocaml-core/latest/doc/base/Base/String/index.html *)
        val extract_line : string -> string * string * string

2.  Vous utiliserez le code suivant comme base et implémenterez la fonction
    `extract` dans le fichier `exercice1/print.ml`
    
        open Base
        open Stdio
        
        (* val extract: In_channel -> (string,string,string) list *)
        (* lit chaque ligne du fichier (In_channel est équivalent du FILE* du langage C) *)
        (* et crée une liste de triplets (forme,catégorie,lemme) *)
        (* pour transformer le ic en string, vous utiliserez les fonctions input_line ou fold_lines *)
        (* cf. la documentation https://ocaml.janestreet.com/ocaml-core/latest/doc/stdio/Stdio/In_channel/index.html *)
        (* vous appellerez ensuite extract_line *)
        let extract _ic = failwith "not implemented"
        
        let () =
          In_channel.create (Sys.get_argv()).(1)
          |> extract
          |> List.iter ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)
    
    Pour compiler, vous utiliserez le gestionnaire de compilation `caml`, avec la
    commande suivante:
    
        dune build print.exe
    
    Il faudra au préalable créer le fichier nommé `dune` de configuration pour le
    gestionnaire avec comme contenu:
    
        (executable
           (name      print)
           (libraries base stdio))
    
    Cela créera un exécutable dans le répertoire `_build/default`. On peut ensuite
    l&rsquo;exécuter:
    
        _build/default/print.exe /chemin/vers/fichier/lefff-3.4.elex
        .... # plein de lignes
        abonda v abonder
        .... # plein de lignes

3.  L&rsquo;implémentation précédente qui repose sur la fonction `extract` souffre d&rsquo;un
    défaut. On doit d&rsquo;abord construire la liste entière avant de l&rsquo;afficher.
    Utiliser les flots vus en cours pour implémenter une version qui peut affiche
    au fur et à mesure que le fichier est lu dans `exercice2/print_stream.ml` à
    partir du code suivant:
    
        open Base
        open Stdio
        
        type 'a stream = Nil | Cons of 'a * 'a stream thunk and 'a thunk = unit -> 'a
        
        (* val extract : In_channel -> (string*string*string) stream *)
        let rec extract _ic = failwith "not implemented"
        
        (* val iter_stream : 'a stream -> ('a -> unit) -> unit *)
        let rec iter_stream _st ~f = failwith "not implemented"
        
        
        let () =
          In_channel.create (Sys.get_argv()).(1)
          |> extract
          |> iter_stream ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)
    
    qu&rsquo;il faudra compiler comme précédemment (ci-dessous le fichier de compilation)
    
        (executable
           (name      print_stream)
           (libraries base stdio))


<a id="org1354858"></a>

# Stocker les mots

On s&rsquo;intéresse pour l&rsquo;instant uniquement aux formes fléchies (la catégorie et le
lemme seront utilisés plus tard). Nous allons stocker les informations dans une
structure de données appelée [Trie](https://fr.wikipedia.org/wiki/Trie_(informatique)).

Un trie est un arbre qui permet de représenter les tables entre chaînes (de
caractères ici, mais on pourrait généraliser) et des valeurs de type quelconque.
Dans un trie les arêtes sont étiquetées par des caractères, et les noeuds
contiennent de l&rsquo;information.

Tout noeud est associé à un mot préfixe que l&rsquo;on étend à ses fils en suivant les
étiquettes des arêtes sortantes. Par exemple si un noeud \(n\) est associé au
préfixe `p`, que \(n\) a pour fils \(n'\) et que l&rsquo;arête les reliant est étiquetée
`a`, alors \(n'\) est associé au préfixe `pa`. Par convention la racine d&rsquo;un trie
est associée à la chaîne vide.

Pour les informations stockées, on va considérer que l&rsquo;on va pouvoir les
représenter par des listes d&rsquo;éléments de type `'a`. Si l&rsquo;information stockée
dans un noeud est vide (type `info`), on considérera que le préfixe n&rsquo;est pas un
mot valide. On va donc accéder aux `tries` avec des termes du type suivant:

    type 'a info = 'a list
    type 'a t = Node of 'a list * 'a arc list and 'a arc = char * 'a t

Dans le cas simple, on veut simplement stocker une liste de mots, le type
d&rsquo;information sera `unit`. Par exemple, si on veut représenter le vocabulaire
contenant *jour,bon,bonjour,bonhomme* on aura le terme:

    - : unit t =
      Node ([],
       [(b,
         Node ([],
          [(o,
            Node ([],
             [(n,
               Node ([()],
                [(j,
                  Node ([],
                   [(o,
                     Node ([],
                      [(u, Node ([], [(r, Node ([()], []))]))]))]));
                 (h,
                  Node ([],
                   [(o,
                     Node ([],
                      [(m,
                        Node ([],
                         [(m, Node ([], [(e, Node ([()], []))]))]))]))]))]))]))]));
        (j,
         Node ([],
          [(o, Node ([], [(u, Node ([], [(r, Node ([()], []))]))]))]))])

On remarque que le préfixe `bon` apparaît une seule fois dans cette structure,
alors que 3 mots le contiennent. On remarque également que le suffixe `jour` est
présent deux fois. Dans un trie, les suffixes ne sont pas partagés à la
différence des préfixes.

On va programmer une bibliothèque de fonctions pour les trie que nous
utiliserons ensuite dans le lemmatiseur.
Comme les tries sont des arbres, nous allons les éditer via un zipper qu&rsquo;il vous
faudra définir. La signature à implémenter est dans le fichier `libtrie/trie.mli`

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

Vous devrez écrire l&rsquo;implémentation dans `libtrie/trie.ml`:

    open Base
    
    (* your code here*)

    (library
      (name        libtrie)
      (public_name        libtrie)
      (libraries base stdio)
      (preprocess (pps ppx_jane ppx_deriving.std)))

Pour tester, vous pouvez modifier le programme suivant
(`exercice3/test_trie.ml`).

    open Libtrie
    open Stdio
    
    let () =
    let t1 = Trie.word_to_trie (Trie.string_to_word "bonjour") [()] in
    let t2 = Trie.insert t1 (Trie.string_to_word "bon")      () in
    let t3 = Trie.insert t2 (Trie.string_to_word "jour")     () in
    let t4 = Trie.insert t3 (Trie.string_to_word "bonhomme") () in
    
    printf "%s\n%!" (Trie.show
                       (* crazy complicated: nothing is designed to print a unit value in ocaml :( *)
                       (fun fmt -> fun _ -> Format.fprintf fmt "()"  )
                       t4)

    (executable
          (name      test_trie)
          (libraries libtrie base stdio))

Pour cet exercice et les suivants, il faut d&rsquo;abord indiquer où se trouve la
bibliothèque `trie` en créant un lien symbolique dans le répertoire source.

    cd exercice3
    ln -s ../libtrie libtrie

Le programme suivant crée un trie à partir du fichier passé en argument de la
ligne de commande puis affiche le contenu du trie, et enfin sauvegarde le résultat dans un fichier binaire appelé `lexicon.bin`.

    open Libtrie
    open Base
    open Stdio
    
    let extract = failwith "not implemented"
    
    let () =
      let lexicon = In_channel.create (Sys.get_argv()).(1)
                    |> extract
                    |> List.fold ~init:(Trie.empty []) ~f:(fun acc (f,_,_) -> Trie.insert acc f ()) in
      let lwords = Trie.extract lexicon in
      let () = List.iter lwords ~f:(fun (s,_) -> printf "%s\n" (Trie.word_to_string s)) in
      let oc = Stdlib.open_out_bin "lexicon.bin" in
      Caml.Marshal.to_channel oc lexicon []

    (executable
          (name      create_lexicon)
          (libraries libtrie base stdio))


<a id="org2ac161d"></a>

## Ajouter le lien entre formes, catégories et lemmes

Pour l&rsquo;instant on a simplement stocké les formes fléchies avec des listes à un
élément `true` pour indiquer la présence dans le dictionnaire. On va maintenant
ajouter l&rsquo;association avec le lemme et la catégorie.

Pour les catégories (nom,verbe, etc&hellip;) on ajoutera explicitement ces informations
(voir plus bas). Pour les lemmes, on va tirer avantage que le lemme est aussi un
mot du lexique pour ne pas l&rsquo;ajouter une fois de plus.

On va tout d&rsquo;abord définir la différence entre deux chaînes: la différence `u -
v` est égale à `w` si la concaténation `w v` est égale à `u`. Soient deux chaînes `w1`
(le lemme dans la suite) et `w2` (la forme fléchie dans la suite) et `p` leur
plus long préfixe commun,  on a que `w1` est égal à `p u1` et
`w2` à `p u2`. Dans ce cas on peut écrire `w1` sous la forme `(w2 - u2) u1`. En
fait on a même pas besoin de connaître `u2` car sa longueur suffit. Par abus de
langage on notera donc `w1` comme `(w2 - |u2|) u1`.
Par exemple `(chantaient - 5) er` est égal à `chanter`

Partant de ces observations, on va représenter les lemmes par une différence et
la concaténation d&rsquo;un suffixe. On supposera que les mots sont représentés par
des listes de caractères. Donner la fonction `diff w1 w2` qui envoie un couple
`(l,s)` telle que `w1` est égal à `(w2 - l) s`, de type:

    type word_diff = int * word
    val diff : word -> word -> word_diff

Donner ensuite la fonction `word_patch w (l,s)` qui effectue l&rsquo;opération inverse
(c&rsquo;est-à-dire retrouve le lemme à partir de la forme fléchie)

    val word_patch : word -> word_diff -> word

Ensuite on va créer le module qui va associer, dans le trie, une forme à son
lemme représenté par la différence entre les deux mots. Le lemmatiseur final se
trouve dans `exercice 5`:

    open Libtrie
    open Base
    open Stdio
    
    type word_diff = int * char list [@@deriving eq]
    type word_diff_cat = {diff: word_diff; cat:string} [@@deriving eq]
    type t = word_diff_cat Trie.t
    
    (* code for diff and word_patch *)
    let diff _l _f = failwith "not implemented"
    let word_patch _w _wd = failwith "not implemented"
    let extract _ic = failwith "not implemented"
    
    let lexicon =
      In_channel.create (Sys.get_argv()).(1)
      |> extract
      |> List.fold
        ~init:(Trie.empty [])
        ~f:(fun acc (f,c,l) ->
            let d = diff (Trie.string_to_word l) (Trie.string_to_word f) in
            let wc = {diff=d;cat=c} in
            if List.mem (Trie.find acc (Trie.string_to_word f)) wc ~equal:equal_word_diff_cat
            then acc else Trie.insert acc (Trie.string_to_word f) wc)
    
    let () = printf "created trie %d\n%!" (Trie.size lexicon);
      let oc = Stdlib.open_out_bin "lexicon.bin" in
      Caml.Marshal.to_channel oc lexicon []
    
    let make_lemmatize t = fun s -> Trie.find t s |> List.map ~f:(fun {diff;cat} -> (word_patch s diff,cat))
    let lemmatizer = make_lemmatize lexicon
    
    
    let rec loop () =
      let () = printf "Entrer forme à lemmatiser:\n" in
      match Stdio.In_channel.input_line Stdio.stdin with
      | None -> ()
      | Some s ->
        let l = Trie.string_to_word s |> lemmatizer in
        let () = List.iter ~f:(fun (l,c)-> printf "%s %s" (Trie.word_to_string l) c) l
        in loop ()
    
    
    let () = loop ()

Et le fichier `dune` associé:

    (executable
          (name      basic_lemmatizer)
          (libraries libtrie base stdio)
          (preprocess (pps ppx_jane ppx_deriving.std)))

