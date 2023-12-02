open BtreeS;;

let rec bst_seek_aux(t, x : 'a t_btree * 'a ) : bool =
  if bt_isEmpty(t)
  then false
  else 
    if bt_root(t) == x
    then true
    else
      if bt_root(t) < x
      then bst_seek_aux(bt_subright(t), x)
      else bst_seek_aux(bt_subleft(t), x)
;;

let bst_seek(t, x : 'a t_btree * 'a) : bool = 
  if bt_isEmpty(t)
  then failwith("error bst_seek : tree is empty")
  else bst_seek_aux(t, x)
;;


let rec bst_linsert(t, x : 'a t_btree * 'a ) : 'a t_btree =
 if bt_isEmpty(t)
 then bt_rooting(x, bt_empty(), bt_empty())
 else
   if bt_root(t) < x
   then bt_rooting(bt_root(t), bt_subleft(t), bst_linsert(bt_subright(t), x))
   else bt_rooting(bt_root(t), bst_linsert(bt_subleft(t), x), bt_subright(t))
;;

let rec bst_lbuild_aux(l, t : 'a list * 'a t_btree) : 'a t_btree =
  if List.length(l)==0
  then t
  else
    let fst : 'a = List.hd(l) in
    bst_lbuild_aux(List.tl(l), bst_linsert(t, fst))
;;

let bst_lbuild(l : 'a list) :'a t_btree =
  if List.length(l)== 0
  then failwith("erreur lbuild : liste vide")
  else bst_lbuild_aux(l, bt_empty())
;;

let rec bst_max(t : 'a t_btree) : 'a =
  if bt_isEmpty(t)
  then failwith("Erreur bst.ml : bst_max : arbre vide")
  else 
    if bt_isEmpty(bt_subright(t))
    then bt_root(t);
    else bst_max(bt_subright(t))
;;

let rec bst_rmMax_aux(t, max : 'a t_btree * 'a): 'a t_btree =
  if bt_isEmpty(t)
  then failwith("Erreur bst.ml : bst_reMax_aux : arbre vide")
  else

let bst_rmMax(t : 'a t_btree) : 'a t_btree =
  if bt_isEmpty(t)
  then failwith("Error bst.ml : mst_reMax : arbre vide")
  else 
    let max: 'a = bstMax(t) in
    bst_rmMax_aux(t, max)
  ;;


let rec bst_delete(t, x : 'a t_btree * 'a ) : 'a t_btree =
 if bt_isEmpty(t)
 then failwith("erreur bst_delete : tree is empty or vaue doesn't exits in the tree")
 else
   if bt_root(t) == x
   then
     let (l,r) : 'a t_btree * 'a t_btree = (bt_subleft(t), bt_subright(t)) in
     if bt_isEmpty(r)
     then
       if bt_isEmpty(l)
       then bt_Empty()
       else l
     else 
      if bt_isEmpty(l)
      then r
      else bt_rooting(bst_max(bt_subleft(t)), bst_suppMax(bt_subleft(t)), bt_subright(t))
   else
      if bt_root(t) < x
      then bt_rooting(bt_root(t), bt_subleft(t), bst_delete(bt_subright(t), x))
      else bt_rooting(bt_root(t), bst_delete(bt_subleft(t), x), bt_subright(t))
;;


let rec print(t : int t_btree) : unit =
  if bt_isEmpty(t)
  then Printf.printf "EMPTY\n"
  else
    (
      let x : int = bt_root(t) in
      Printf.printf "%d\n%!" x;
      Printf.printf "gauche";
      print(bt_subleft(t));
      Printf.printf "droite";
      print(bt_subright(t));
    )
;;
