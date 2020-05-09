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
