open BtreeS ;;
open Bst ;;

val right_rotate : 'a t_btree -> 'a t_btree

val left_rotate : 'a t_btree -> 'a t_btree

val right_left_rotate : 'a t_btree -> 'a t_btree

val left_right_rotate : 'a t_btree -> 'a t_btree