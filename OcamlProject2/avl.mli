open BtreeS ;;
open Bst ;;

type avl

val right_rotate : 'a t_btree -> 'a t_btree

val left_rotate : 'a t_btree -> 'a t_btree

val right_left_rotate : 'a t_btree -> 'a t_btree

val left_right_rotate : 'a t_btree -> 'a t_btree

val imbalance : 'a t_btree -> int

val imbalanceUpdate : 'a t_btree -> 'a t_btree

val rebalance_aux : 'a t_btree -> 'a t_btree

val rebalance : 'a t_btree -> 'a t_btree