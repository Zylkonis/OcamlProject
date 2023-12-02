#load "btreeS.cmo";;
open BtreeS;;
#load "bst.cmo";;
open Bst;;
open Random ;;


(* QUESTION 1 *)
let RandInit = Random.self_init ();;

let rec bst_rnd_create () : 'a t_btree =