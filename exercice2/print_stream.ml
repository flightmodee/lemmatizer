open Base
open Stdio

type 'a stream = Nil | Cons of 'a * 'a stream thunk and 'a thunk = unit -> 'a


let getOption v = match v with
	| None -> failwith "Empty option"
	| Some e -> e
	
let split_aux (sl:string list) : (string * string * string) = match sl with
	| a::_::c::_::e::_ -> (a,c, getOption (List.hd (String.split e ~on:'_')))
	| _ -> failwith "This shouldn't have happened. Stop. Get some help."

let extract_line (line:string) : (string*string*string) = let l = String.split line ~on:'\t' in split_aux l;;


let rec extract_rec ic  = let line = Stdio.In_channel.input_line ic in match line with
	| Some _ -> let triplet = getOption line |> extract_line in triplet::(extract_rec ic)
	| None -> []


(* val extract : In_channel -> (string*string*string) stream *)
let rec extract_stream ic = let line = Stdio.In_channel.input_line ic in match line with
	| None -> Nil
	| Some _ -> let triplet = getOption line |> extract_line in Cons(triplet, fun () -> extract_stream ic)
	

(* val iter_stream : 'a stream -> ('a -> unit) -> unit *)
let rec iter_stream st ~f:f = match st with
	| Nil -> ()
	| Cons (e,q) -> f e; iter_stream (q ()) ~f:f



let () =
  In_channel.create (Sys.get_argv()).(1)
  |> extract_stream
  |> iter_stream ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)
