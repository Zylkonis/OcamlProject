#load "avl.cmo" ;;
open Avl;;


(* Exercice 2. Expérimentations avec les arbres AVL *)

(* Question 1 *)


let avl_rnd_create(nbElt, borne : int * int) : 'a t_btree =
  let t : 'a t_btree ref = ref (bt_empty()) in
  let rand: int ref = ref 0 in
  while (bt_size(!t) < nbElt) do
    rand := Random.int(borne);
    t := avl_insert(!t, !rand)
  done;
  !t
;;


let rec cpx_avl_seek_aux(t, x, cpx : 'a t_btree * 'b * int) : int =
  if bt_isempty(t)
  then cpx
  else
    let (value, balance) : 'b * int = bt_root(t) in
    if value = x
    then cpx
    else
      if value < x
      then cpx_avl_seek_aux(bt_subright(t), x, cpx + 1)
      else cpx_avl_seek_aux(bt_subleft(t), x, cpx + 1)
;;

let cpx_avl_seek(t, x : 'a t_btree * 'b) : int = 
  if bt_isempty(t)
  then failwith("error bst_seek : tree is empty")
  else cpx_avl_seek_aux(t, x, 0)
;;


let rec cpx_avl_insert_aux(t, value, cpx : 'a t_btree * 'b * int) : int =
  if bt_isempty(t)
  then cpx
  else
    let (r, imb) : 'b * int = bt_root(t) in
    if r = value
    then cpx
    else
      if r < value
      then cpx_avl_insert_aux(bt_subright(t), value, cpx + 1)
      else cpx_avl_insert_aux(bt_subleft(t), value, cpx + 1)
;;

let cpx_avl_insert(t, value : 'a t_btree * 'b) : int =
  if bt_isempty(t)
  then 0
  else
    let cpx : int ref = ref 0 in
    cpx := cpx_avl_insert_aux(t, value, !cpx);
    (*rebalance(imbalanceUpdate(!tree)) *)
    !cpx
;;


let rec cpx_avl_delete_aux(t, x, cpx : 'a t_btree * 'b * int) : int =
  if bt_isempty(t)
  then cpx
  else
    let (value,balance) = bt_root(t)
    in
    if value = x
    then
      let (l,r) : 'a t_btree * 'a t_btree = (bt_subleft(t), bt_subright(t)) in
      if bt_isempty(r)
      then
        if bt_isempty(l)
        then cpx
        else cpx
      else 
       if bt_isempty(l)
       then cpx
       else cpx
    else
       if value < x
       then cpx_avl_delete_aux(bt_subright(t), x, cpx + 1)
       else cpx_avl_delete_aux(bt_subleft(t), x, cpx + 1)
 ;;

 let cpx_avl_delete(t, x : 'a t_btree * 'b ) : int =
   if bt_isempty(t)
   then failwith("erreur cpx_avl_delete : tree is empty or value doesn't exits in the tree")
   else cpx_avl_delete_aux(t, x, 0)
    (*let tree : 'a t_btree ref = ref t in
    let final_tree : 'a t_btree ref = ref (bt_empty()) in
    tree := cpx_avl_delete_aux(!tree, x);
    while (!tree <> !final_tree) do
      final_tree := !tree;
      tree := rebalance(imbalanceUpdate(!tree))
    done;
    !tree*)
;;



let randTree : 'a t_btree ref = ref (bt_empty()) in
let borne : int = 1000 in
for i = 0 to 100 do
  randTree := avl_rnd_create(20, borne);
  Printf.printf "complexité avl_seek, arbre de taille 20 = %d\n" cpx_avl_seek(!randTree, borne);
  Printf.printf "complexité avl_insert, arbre de taille 20 = %d\n" cpx_avl_insert(!randTree, borne);
  Printf.printf "complexité avl_delete, arbre de taille 20 = %d\n" cpx_avl_delete(!randTree, borne);
  randTree := avl_rnd_create(50, borne);
  Printf.printf "complexité avl_seek, arbre de taille 50 = %d\n" cpx_avl_seek(!randTree, borne);
  Printf.printf "complexité avl_insert, arbre de taille 50 = %d\n" cpx_avl_insert(!randTree, borne);
  Printf.printf "complexité avl_delete, arbre de taille 50 = %d\n" cpx_avl_delete(!randTree, borne);
  randTree := avl_rnd_create(100, borne);
  Printf.printf "complexité avl_seek, arbre de taille 100 = %d\n" cpx_avl_seek(!randTree, borne);
  Printf.printf "complexité avl_insert, arbre de taille 100 = %d\n" cpx_avl_insert(!randTree, borne);
  Printf.printf "complexité avl_delete, arbre de taille 100 = %d\n" cpx_avl_delete(!randTree, borne);
done;