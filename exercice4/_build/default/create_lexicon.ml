open Libtrie
open Base
open Stdio


let getOption v = match v with
	| Some e -> e
	| None -> failwith "option is none";;

let split_aux (sl:string list) : (string * string * string) = match sl with
	| a::_::c::_::e::_ -> (a,c, getOption (List.hd (String.split e ~on:'_')))
	| _ -> failwith "This shouldn't have happened. Stop. Get some help.";;
	

let extract_line (line:string) : (string*string*string) = let l = String.split line ~on:'\t' in split_aux l;;


let rec extract_rec ic  = let line = Stdio.In_channel.input_line ic in match line with
	| None -> []
	| Some _ -> let triplet = getOption line |> extract_line in triplet::(extract_rec ic)
;;

let extract ic = extract_rec ic;;


let () =
  let lexicon = In_channel.create (Sys.get_argv()).(1)
                |> extract
                |> List.fold ~init:(Trie.empty []) ~f:(fun acc (f,_,_) -> Trie.insert acc (Trie.string_to_word f) ()) in
  let lwords = Trie.extract lexicon in
  let () = List.iter lwords ~f:(fun (s,_) -> printf "%s\n" (Trie.word_to_string s)) in
  let oc = Stdlib.open_out_bin "lexicon.bin" in
  Caml.Marshal.to_channel oc lexicon []
