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


let bst_rnd_create(nbElt : int) : 'a t_btree =
  let borne : int = 101 in
  let t : 'a t_btree ref = ref (bt_empty()) in
  let rand: int ref = ref 0 in
  while (bt_size(!t) < nbElt) do
    rand := Random.int(borne);
    t := bst_linsert(!t, !rand)
  done;
  !t
;;

(* QUESTION 2 *)

let rec getHeight(t : 'a t_btree) : int =
  if(bt_isempty(t))
  then 0
  else 1 + max getHeight(bt_subleft(t)) getHeight(bt_subright(t))
;;

let rec imbalance_aux(t: 'a t_btree) : int =
  if bt_isempty(t)
  then
  getHeight(bt_subleft(t)) - getHeight(bt_subright(t)) + imbalance_aux(bt_subleft(t)) + imbalance_aux(bt_subright(t))
;;

let  imbalance(t: 'a t_btree) : int =
  if bt_isempty(t)
  then 0
  imbalance_aux(t) / getSize(t)
;;

let  imbalance(t : 'a t_btree) : int =
  if bt_isempty(t)
  then 0
  else getHeight(bt_subleft) - getHeight(bt_subright)
  


let bt_imbalence(t : 'a tbtree) : int =
if bt_isempty(t)
  then 0
  else getHeight(bt_subleft) - getHeight(bt_subright)
  ;;;;

let avgImbalance(nbTree: int): int =
  let avg : int ref = ref 0 in
  for i = 0 to nbTree do
    avg := !avg + imbalance(bst_rnd_create(25))
  done;
  !avg / nbTree
;;


let rec getHeight(t : 'a t_btree) : int =
  if(bt_isempty(t))
  then 0
  else 1 + max (getHeight(bt_subleft(t))) (getHeight(bt_subright(t)))
;;

let imbalance(t: 'a t_btree) : int =
  abs(getHeight(bt_subleft(t)) - getHeight(bt_subright(t)))
;;

let avgImbalance(nbTree: int): float =
  let avg : int ref = ref 0 in
  for i = 1 to nbTree do
    avg := !avg + imbalance(bst_rnd_create(6))
  done;
  let average : float = (float_of_int(!avg))/.(float_of_int(nbTree)) in
  Printf.printf "%f\n" average;
  average
;;



