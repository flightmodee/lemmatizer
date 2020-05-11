open Base
open Stdio

type 'a stream = Nil | Cons of 'a * 'a stream thunk and 'a thunk = unit -> 'a

(* val extract : In_channel -> (string*string*string) stream *)
let rec extract_stream ic = let line = Stdio.In_channel.input_line ic in match line with
	| None -> Nil
	| Some e -> let triplet = getOption line |> extract_line in Cons(triplet, fun () -> extract_stream ic)
;;
	

(* val iter_stream : 'a stream -> ('a -> unit) -> unit *)
let rec iter_stream st ~f:f = match st with
	| Nil -> ()
	| Cons (e,q) -> f e; iter_stream (q ()) f
;;



let () =
  In_channel.create (Sys.get_argv()).(1)
  |> extract_stream
  |> iter_stream ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)
