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


let insert_complexite(nbTree : int)




