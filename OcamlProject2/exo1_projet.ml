#load "btreeS.cmo";;
open BtreeS ;;
#load "bst.cmo";;
open Bst ;;
open Random ;;


(* QUESTION 1 *)
let randInit  = Random.self_init();;


let rec bt_size(t : 'a t_btree): int =
  if bt_isempty(t)
  then 0
  else
    1 + bt_size(bt_subleft(t)) + bt_size(bt_subright(t))
;;

let rec bst_rnd_create(nbElt : int) : 'a t_btree =
  let borne : int = 51 in
  let t : t_btree = empty()
  while (bt_size(t) <> nbElt) do
    let rand : int = Random.int(borne) in
    t = bst_linsert(rand)
  done;
  t
;;

















