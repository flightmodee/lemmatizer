open Base
open Stdio

(* val extract: In_channel -> (string,string,string) list *)
(* lit chaque ligne du fichier (In_channel est équivalent du FILE* du langage C) *)
(* et crée une liste de triplets (forme,catégorie,lemme) *)
(* pour transformer le ic en string, vous utiliserez les fonctions input_line ou fold_lines *)
(* cf. la documentation https://ocaml.janestreet.com/ocaml-core/latest/doc/stdio/Stdio/In_channel/index.html *)
(* vous appellerez ensuite extract_line *)
let extract ic = failwith "not implemented"

let () =
  In_channel.create (Sys.get_argv()).(1)
  |> extract
  |> List.iter ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)
