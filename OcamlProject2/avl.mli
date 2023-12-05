open BtreeS ;;
open Bst ;;

(* AVL = Arbre Binaire de Recherche Equilibré *)

(* Fonction appliquant une rotation à droite *)
val right_rotate : 'a t_btree -> 'a t_btree

(* Fonction appliquant une rotation à gauche *)
val left_rotate : 'a t_btree -> 'a t_btree

(* Fonction appliquant une rotation droite-gauche *)
val right_left_rotate : 'a t_btree -> 'a t_btree

(* Fonction appliquant une rotation gauche-droite *)
val left_right_rotate : 'a t_btree -> 'a t_btree

(* Focntion calculant l'équilibre d'un noeud *)
val imbalance2 : 'a t_btree -> int

(* Fonction parcourant un AVL afin d'en déterminer l'équilibre en chaque noeud *)
val imbalanceUpdate : 'a t_btree -> 'a t_btree

(* Fonction auxiliaire de rééquilibrage d'un AVL *)
val rebalance_aux : 'a t_btree -> 'a t_btree

(* Fonction de rééquilibrage d'un AVL *)
val rebalance : 'a t_btree -> 'a t_btree

(* Fonction d'affichage d'un AVL *)
val printavl : 'a t_btree -> unit

(* Fonction auxiliaire d'insertion d'un élément dans un AVL *)
val avl_insert_aux : 'a t_btree * 'b -> 'a t_btree

(* Fonction d'insertion dans un AVL *)
val avl_insert : 'a t_btree * 'b -> 'a t_btree

(* Fonction retournant l'élément maximal d'un AVL *)
val avl_max : 'a t_btree -> 'b

(* Fonction supprimant l'élément maximal d'un AVL *)
val avl_rmMax : 'a t_btree -> 'a t_btree

(* Fonction auxiliaire de suppression d'un élément dans un AVL *)
val avl_delete_aux : 'a t_btree * 'b -> 'a t_btree

(* Fonction de suppression d'un élément dans un AVL *)
val avl_delete : 'a t_btree * 'b -> 'a t_btree

(* Fonction auxiliaire de recherche d'un élément dans un AVL *)
val avl_seek_aux : 'a t_btree * 'b  -> bool

(*Fonction de recherche d'un élément dans un AVL *)
val avl_seek 'a t_btree * 'b -> bool
