#load "btreeS.cmo" ;;
open BtreeS ;;
#load "bst.cmo" ;;
open Bst ;;

type 'a t_avl = 'a t_btree * 'a


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



let hasImbalance (t : 'a t_avl) : 'a t_avl =
  let (tree,balance) : 'a avl = t and
  let (subleft_tree, subleft_balance) : 'a avl :,
  if (balance = 2) && 

  


