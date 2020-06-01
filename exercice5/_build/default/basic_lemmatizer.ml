open Libtrie
open Base
open Stdio

type word_diff = int * char list [@@deriving eq]
type word_diff_cat = {diff: word_diff; cat:string} [@@deriving eq]
type t = word_diff_cat Trie.t


let word_length w = Trie.word_to_string w |> String.length

let rec diff_aux lemme ff = match ff with
	| [] -> (0, lemme)
	| e::reste_ff -> match lemme with
				| [] -> (word_length (e::reste_ff), [])
				| chr::reste_lemme -> if (Char.equal chr e) 
									  then (diff_aux reste_lemme reste_ff)
									  else (word_length (e::reste_ff), chr::reste_lemme)


let diff lemme ff = diff_aux lemme ff




let word_patch w wd = match wd with
	| (0, []) -> w
	| (l, s) -> let (pre, _) = List.split_n w (List.length w - l)  in (Trie.word_to_string pre)^(Trie.word_to_string s) |> Trie.string_to_word	
	
	

let getOption v = match v with
	| Some e -> e
	| None -> failwith "option is none"





let split_aux (sl:string list) : (string * string * string) = match sl with
	| a::_::c::_::e::_ -> (a,c, getOption (List.hd (String.split e ~on:'_')))
	| _ -> failwith "This shouldn't have happened. Stop. Get some help."





let extract_line (line:string) : (string*string*string) = let l = String.split line ~on:'\t' in split_aux l



let rec extract_rec ic  = let line = Stdio.In_channel.input_line ic in match line with
	| Some _ -> let triplet = getOption line |> extract_line in triplet::(extract_rec ic)
	| None -> []


let extract ic = extract_rec ic




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
  let () = printf "\nEntrer forme à lemmatiser:\n%!" in
  match Stdio.In_channel.input_line Stdio.stdin with
  | None -> ()
  | Some s ->
    let l = Trie.string_to_word s |> lemmatizer in
    let () = List.iter ~f:(fun (l,c)-> printf "%s %s" (Trie.word_to_string l) c) l
    in loop ()


let () = loop ()
