open Base

(* your code here*)

type 'a t = Node of 'a list * ('a arc list) [@@deriving show] and 'a arc = char * 'a t [@@deriving show]
