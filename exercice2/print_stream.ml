open Base
open Stdio

type 'a stream = Nil | Cons of 'a * 'a stream thunk and 'a thunk = unit -> 'a

(* val extract : In_channel -> (string*string*string) stream *)
let rec extract ic = failwith "not implemented"

(* val iter_stream : 'a stream -> ('a -> unit) -> unit *)
let rec iter_stream st ~f = failwith "not implemented"


let () =
  In_channel.create (Sys.get_argv()).(1)
  |> extract
  |> iter_stream ~f:(fun (f,c,l) -> printf "%s %s %s\n" f c l)
