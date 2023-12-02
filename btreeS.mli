(** type for binary trees *)
type 'a t_btree

(** create empty tree *)
val bt_empty: unit -> 'a t_btree

(** create the root and sons *)
val bt_rooting: 'a * 'a t_btree * 'a t_btree -> 'a t_btree

(** check if the root has a value *)
val bt_isEmpty: 'a t_btree -> bool

(** get the value of the root *)
val bt_root: 'a t_btree -> 'a

(** passer au fils gauche *)
val bt_subleft: 'a t_btree -> 'a t_btree

(** passer au fils droit *)
val bt_subright: 'a t_btree -> 'a t_btree                                
