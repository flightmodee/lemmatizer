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
