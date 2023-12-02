type 'a t_btree = EMPTY
                 | ROOT of 'a * 'a t_btree * 'a t_btree
;;

let bt_empty() :'a t_btree = EMPTY;;

let bt_rooting(value, l_tree, r_tree: 'a * 'a t_btree * 'a t_btree): 'a t_btree = ROOT(value, l_tree, r_tree);;

let bt_isEmpty(root: 'a t_btree): bool =
  match root with
  | EMPTY -> true
  | _ -> false
;;

let bt_root(root: 'a t_btree): 'a =
  match root with
  | EMPTY -> failwith ("arbre vide")
  | ROOT(value, l_tree, r_tree) -> value
;;

(** passer au fils gauche *)
let bt_subleft(root: 'a t_btree): 'a t_btree =
  match root with
  | EMPTY -> failwith ("arbre vide")
  | ROOT(r, g, d) -> g
;;

(** passer au fils droit *)
let bt_subright(root: 'a t_btree): 'a t_btree =
  match root with
  | EMPTY -> failwith ("arbre vide")
  | ROOT (r, g, d) -> d
;;
