open Libtrie
open Base
open Stdio

type word_diff = int * char list [@@deriving eq]
type word_diff_cat = {diff: word_diff; cat:string} [@@deriving eq]
type t = word_diff_cat Trie.t

(* code for diff and word_patch *)
let diff l f = failwith "not implemented"
let word_patch w wd = failwith "not implemented"
let extract ic = failwith "not implemented"

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
