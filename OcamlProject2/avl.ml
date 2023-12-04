#load "btreeS.cmo" ;;
open BtreeS ;;
#load "bst.cmo" ;;
open Bst ;;

type t_avl : int * int = (val1, val2);;

let left_rotate (t : 'a t_btree) : 'a t_btree =
  let new_t : 'a t_btree ref = ref (bt_empty()) in
  new_t := bt_rooting(bt_root(bt_subright(t)), 
                      bt_rooting(bt_root(t), bt_subleft(t), bt_subleft(bt_subright(t))), 
                      bt_subright(bt_subright(t)));
  !new_t
;;

let right_rotate (t : 'a t_btree) : 'a t_btree =
  let new_t : 'a t_btree ref = ref (bt_empty()) in
  new_t := bt_rooting(bt_root(bt_subleft(t)),
                               bt_subleft(bt_subleft(t)),
                               bt_rooting(bt_root(t), bt_subright(bt_subleft(t)), bt_subright(t)));
  !new_t
;;


let right_left_rotate (t : 'a t_btree) : 'a t_btree =
  let new_t : 'a t_btree ref = ref (bt_empty()) in
  new_t := bt_rooting(bt_root(t), bt_subleft(t), right_rotate(bt_subright(t)));
  new_t := left_rotate(!new_t);
  !new_t
;;



let left_right_rotate (t : 'a t_btree) : 'a t_btree =
  let new_t : 'a t_btree ref = ref (bt_empty()) in
  new_t := bt_rooting(bt_root(t), left_rotate(bt_subleft(t)), bt_subright(t));
  new_t := right_rotate(!new_t);
  !new_t
;;

let rec rebalance_aux(t : 'a t_btree) : 'a t_btree =
  let (value, balance) = bt_root(t) in
  let (value_subleft, balance_subleft) = bt_root(bt_subleft(t)) in
  let (value_subright, balance_subright) = bt_root(bt_subright(t)) in
  let new_t : 'a t_btree ref = ref t in
  if (balance = 2) && (balance_subleft = -1)
  then new_t := right_rotate(!new_t)
  else
    if (balance = 2) && (balance_subleft = 1)
    then new_t := left_right_rotate(!new_t)
    else 
      if (balance = -2) && (balance_subright = -1)
      then new_t := left_rotate(!new_t)
      else 
        if (balance = -2) && (balance_subright = 1)
        then new_t := right_left_rotate(!new_t)
        else 
          if (balance = 2)
          then new_t := rebalance(bt_subleft(!new_t))
          else new_t := rebalance(bt_subright(!new_t));
  !new_t
;;

let rebalance(t: 'a t_btree) : 'a t_btree =
  let (value, balance) = bt_root(t) in
  let tree : 'a t_btree ref = ref t in
  if (balance = -1 || balance = 0 || balance = 1)
  then tree := bt_rooting(bt_root(!tree), rebalance(bt_subleft(!tree)), rebalance(bt_subright(!tree)))
  else tree := rebalance_aux(!tree)
;;


let imbalance(t: 'a t_btree) : int  =
  if bt_isempty(t)
  then 0
  else getHeight(bt_subleft(t)) - getHeight(bt_subright(t))
;;

let rec imbalanceUpdate(t: 'a t_btree) : 'a t_btree =
  if bt_isempty(t)
  then bt_empty()
  else 
    let r, imb : int * int = bt_root(t) in
    let newImb : int = imbalance(t) in
    bt_rooting((r, newImb), imbalanceUpdate(bt_subleft(t)), imbalanceUpdate(bt_subright(t)))
;;
  




