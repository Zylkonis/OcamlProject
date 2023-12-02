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


let bst_rnd_create(nbElt, borne : int * int) : 'a t_btree =
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
  else 1 + max (getHeight(bt_subleft(t))) (getHeight(bt_subright(t)))
;;


let rec imbalance_aux(t: 'a t_btree) : int =
  if bt_isempty(t)
  then 0
  else getHeight(bt_subleft(t)) - getHeight(bt_subright(t)) + imbalance_aux(bt_subleft(t)) + imbalance_aux(bt_subright(t))
;;

let  imbalance(t: 'a t_btree) : float =
  if bt_isempty(t)
  then 0.0
  else (float_of_int(imbalance_aux(t))) /. (float_of_int(bt_size(t)))
;;

let avgImbalance(nbTree: int): float =
  let avg : float ref = ref 0.0 in
  for i = 1 to nbTree do
    avg := !avg +. imbalance(bst_rnd_create(25))
  done;
  let average : float = !avg/.(float_of_int(nbTree)) in
  Printf.printf "%f\n" average; 
  average
;
(*PRECIEUX BONNE VERSION C LA QUESTION 2*)
let  imbalance(t : 'a t_btree) : int =
  if bt_isempty(t)
  then 0
  else getHeight(bt_subleft) - getHeight(bt_subright)
;;

let rec bt_imbalance(t : 'a tbtree) : int =
  if bt_isempty(t)
  then 0
  else imbalance(t) + bt_imbalence(bt_subright(t)) + bt_imbalence(bt_subl(t))
;;

let avgImbalance(nb_bt, nbsize, nb_Max: int * int * int) : float =
let res : int ref = ref 0. in
for i = 0 to nb_bt - 1 do
  let bt : 'a t_btree = bst_rnd_create(nbsize, nb_Max) in
  res := !res +. float_of_int(bt_imbalance(bt) /. float_of_int(nbsize));
done;
(!res /. float_of_int(nb_bt))
;;
(*PRECIEUX BONNE VERSION C LA QUESTION 2*)
(*PRECIEUX BONNE VERSION*);;

let avgImbalance(nbTree: int): int =
  let avg : int ref = ref 0 in
  for i = 0 to nbTree do
    avg := !avg + imbalance(bst_rnd_create(25))
  done;
  !avg / nbTree
;;
(*PRECIEUX BONNE VERSION*)
(*
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
*)


