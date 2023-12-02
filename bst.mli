open BtreeS;;

val bst_seek_aux : 'a t_btree * 'a -> bool ;;

val bst_seek : 'a t_btree * 'a -> bool;;

val bst_linsert : 'a t_btree * 'a -> 'a t_btree;;

val bst_lbuild_aux : 'a list * 'a t_btree -> 'a t_btree;;

val bst_lbuild : 'a list -> 'a t_btree;;

val bst_delete : 'a t_btree * 'a -> 'a t_btree ;;

val print : int t_btree -> unit ;;
