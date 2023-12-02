#load "btreeS.cmo";;
open BtreeS;;
#load "bst.cmo";;
open Bst;;
let test : int t_btree = bt_rooting(4, 
                                    bt_rooting(2, 
                                               bt_rooting(1, 
                                                          bt_empty(), 
                                                          bt_empty()), 
                                               bt_rooting(3, 
                                                          bt_empty(), 
                                                          bt_empty())), 
                                    bt_rooting(6, 
                                               bt_rooting(5, 
                                                          bt_empty(),
                                                          bt_empty()) ,
                                               bt_rooting(7,bt_empty(), 
                                                          bt_empty()) ));;

let t2 : 'a t_btree = bst_linsert(test, 9);;

let t3 : 'a list = [4;2;1;3;6;5;7];;

let t4 : 'a t_btree = bst_delete(test,7);;

let rec bst_isBst_aux(t, res : 'a t_btree * bool) : bool =
  if bt_isempty(bt_subleft(t))
  then
    if bt_isempty(bt_subright(t))
    then res
    else
      if bt_root(t) < bt_root(bt_subright(t))
      then bst_isBst_aux(bt_subright(t), res)
      else
        (
          res = false;
          res;
        )
  else
    if bt_isempty(bt_subright(t))
    then
      if bt_root(t) > bt_root(bt_subleft(t))
      then bst_isBst_aux(bt_subleft(t), res)
      else
        (
          res = false;
          res;
        )
    else
      let (x,y) : bool * bool = (bst_isBst_aux(bt_subleft(t), res), bst_isBst_aux(bt_subright(t), res)) in
      if x == false || y == false
      then false
      else true
;;
