#load "btreeS.cmo" ;;
open BtreeS ;;
#load "bst.cmo" ;;
open Bst ;;


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

let imbalance2(t: 'a t_btree) : int  =
  if bt_isempty(t)
  then 0
  else getHeight(bt_subleft(t)) - getHeight(bt_subright(t))
;;

let rec imbalanceUpdate(t: 'a t_btree) : 'a t_btree =
  if bt_isempty(t)
  then bt_empty()
  else 
    let (r, imb) : int * int = bt_root(t) in
    let newImb : int = imbalance2(t) in
    bt_rooting((r, newImb), imbalanceUpdate(bt_subleft(t)), imbalanceUpdate(bt_subright(t)))
;;

let rec rebalance_aux(t : 'a t_btree) : 'a t_btree =
  let (value, balance) : int * int = bt_root(t) in
  let new_t : 'a t_btree ref = ref (bt_rooting(bt_root(t), bt_subleft(t), bt_subright(t))) in
  if (balance = 2)
  then (
      let (value_subleft, balance_subleft) : int * int = bt_root(bt_subleft(t)) in
      if (balance_subleft = -1)
      then new_t := left_right_rotate(!new_t)
      else 
        if (balance_subleft = 1)
        then new_t := right_rotate(!new_t)
        else new_t := bt_rooting(bt_root(!new_t), rebalance_aux(bt_subleft(!new_t)), bt_subright(!new_t))
  )
  else (
    let (value_subright, balance_subright): int * int = bt_root(bt_subright(t)) in
    if (balance_subright = -1)
    then new_t := left_rotate(!new_t)
    else 
      if (balance_subright = 1)
      then new_t := right_left_rotate(!new_t)
      else new_t := bt_rooting(bt_root(!new_t), bt_subleft(!new_t), rebalance_aux(bt_subright(!new_t)))
    );
  !new_t
;;

let rec rebalance(t: 'a t_btree) : 'a t_btree =
  if bt_isempty(t)
  then bt_empty()
  else let (value, balance) = bt_root(t) in
       let tree : 'a t_btree ref = ref t in
       if (balance = -1 || balance = 0 || balance = 1)
       then tree := bt_rooting(bt_root(!tree), rebalance(bt_subleft(!tree)), rebalance(bt_subright(!tree)))
       else tree := rebalance_aux(!tree);
  !tree
;;


  
let rec avl_insert(t, val : 'a t_btree * 'a) : 'a t_btree =
  if bt_isempty(t)
  then bt_rooting((val, 0))
  else
    let r, imb : 'a * int = bt_root(t) in
    if r <> val
    then t
    else
      if r < val
      then rt_rooting((r, imb - 1), bt_subleft(t), avl_insert(bt_subright(t), val))
      else rt_rooting((r, imb + 1), avl_insert(bt_subleft(t), val), bt_subright(t))
;;

let avl_max(t : 'a t_btree) : 'a =
  if bt_isempty(bt_subright(t))
  then
    let r, imb : 'a * int = bt_root(t) in
    r
  else 

let rec avl_delete(t, x : 'a t_btree * 'a ) : 'a t_btree =
  if bt_isempty(t)
  then failwith("erreur avl_delete : tree is empty or value doesn't exits in the tree")
  else
    if bt_root(t) = x
    then
      let (l,r) : 'a t_btree * 'a t_btree = (bt_subleft(t), bt_subright(t)) in
      if bt_isempty(r)
      then
        if bt_isempty(l)
        then bt_empty()
        else l
      else 
       if bt_isempty(l)
       then r
       else bt_rooting(avl_max(bt_subleft(t)), avl_rmMax(bt_subleft(t)), bt_subright(t))
    else
       if bt_root(t) < x
       then bt_rooting(bt_root(t), bt_subleft(t), bst_delete(bt_subright(t), x))
       else bt_rooting(bt_root(t), bst_delete(bt_subleft(t), x), bt_subright(t))
 ;;








(*CODE A JOUR MAIS MARCHE PAS*)
let imbalance2(t: 'a t_btree) : int  =
  if bt_isempty(t)
  then 0
  else getHeight(bt_subleft(t)) - getHeight(bt_subright(t))
;;

let rec imbalanceUpdate(t: 'a t_btree) : 'a t_btree =
  if bt_isempty(t)
  then bt_empty()
  else 
    let (r, imb) : int * int = bt_root(t) in
    let newImb : int = imbalance2(t) in
    bt_rooting((r, newImb), imbalanceUpdate(bt_subleft(t)), imbalanceUpdate(bt_subright(t)))
;;

let rec rebalance_aux(t : 'a t_btree) : 'a t_btree =
  let (value, balance) : int * int = bt_root(t) in
  let new_t : 'a t_btree ref = ref (bt_rooting(bt_root(t), bt_subleft(t), bt_subright(t))) in
  if (balance = 2)
  then (
      let (value_subleft, balance_subleft) : int * int = bt_root(bt_subleft(t)) in
      if (balance_subleft = -1)
      then new_t := right_rotate(!new_t)
      else 
        if (balance_subleft = 1)
        then new_t := left_right_rotate(!new_t)
        else new_t := bt_rooting(bt_root(!new_t), rebalance_aux(bt_subleft(!new_t)), bt_subright(!new_t))
  )
  else (
    let (value_subright, balance_subright): int * int = bt_root(bt_subright(t)) in
    if (balance_subright = -1)
    then new_t := left_rotate(!new_t)
    else 
      if (balance_subright = 1)
      then new_t := right_left_rotate(!new_t)
      else new_t := bt_rooting(bt_root(!new_t), bt_subleft(!new_t), rebalance_aux(bt_subright(!new_t)))
    );
  !new_t
;;

let rec rebalance(t: 'a t_btree) : 'a t_btree =
  if bt_isempty(t)
  then bt_empty()
  else let (value, balance) = bt_root(t) in
       let tree : 'a t_btree ref = ref t in
       if (balance = -1 || balance = 0 || balance = 1)
       then tree := bt_rooting(bt_root(!tree), rebalance(bt_subleft(!tree)), rebalance(bt_subright(!tree)))
       else tree := rebalance_aux(!tree);
  !tree
;;

let rec printavl(t : 'a t_btree) : unit =
  if bt_isempty(t)
  then Printf.printf "EMPTY\n"
  else
    (
      let (x,y) : int*int = bt_root(t) in
      Printf.printf " %d , %d\n%!" x y ;
      Printf.printf "gauche";
      printavl(bt_subleft(t));
      Printf.printf "droite";
      printavl(bt_subright(t));
    )
;;