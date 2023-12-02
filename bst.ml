open BtreeS;;

let rec bst_seek_aux(t, x : 'a t_btree * 'a ) : bool =
  if bt_isempty(t)
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
  if bt_isempty(t)
  then failwith("error bst_seek : tree is empty")
  else bst_seek_aux(t, x)
;;


let rec bst_linsert(t, x : 'a t_btree * 'a ) : 'a t_btree =
 if bt_isempty(t)
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

let rec bst_delete(t, x : 'a t_btree * 'a ) : 'a t_btree =
 if bt_isempty(t)
 then failwith("erreur bst_delete : tree is empty or vakue doesn't exits in the tree")
 else
   if bt_root(t) == x
   then
     let (l,r) : 'a t_btree * 'a t_btree = (bt_subleft(t), bt_subright(t)) in
     if bt_isempty(r)
     then
       if bt_isempty(l)
       then bt_empty()
       else bt_rooting(bt_root(l), bt_subleft(l), bt_subright(l))
     else bt_rooting(bt_root(r), l, bt_subright(r))
   else
      if bt_root(t) < x
      then bt_rooting(bt_root(t), bt_subleft(t), bst_delete(bt_subright(t), x))
      else bt_rooting(bt_root(t), bst_delete(bt_subleft(t), x), bt_subright(t))
;;

let rec print(t : int t_btree) : unit =
  if bt_isempty(t)
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
