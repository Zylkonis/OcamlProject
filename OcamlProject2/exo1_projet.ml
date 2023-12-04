#load "btreeS.cmo";;
open BtreeS ;;
#load "bst.cmo";;
open Bst ;;
open Random ;;


(* QUESTION 1 *)
let randInit  = Random.self_init();;


let rec bt_size(t : 'a t_btree): int =
  if bt_isempty(t)
  then 0
  else
    1 + bt_size(bt_subleft(t)) + bt_size(bt_subright(t))
;;


let bst_rnd_create(nbElt, borne : int * int) : 'a t_btree =
  let t : 'a t_btree ref = ref (bt_empty()) in
  let rand: int ref = ref 0 in
  while (bt_size(!t) < nbElt) do
    rand := Random.int(borne);
    t := bst_linsert(!t, !rand)
  done;
  !t
;;

(* QUESTION 2 *)


let rec getHeight(t : 'a t_btree) : int =
  if(bt_isempty(t))
  then 0
  else 1 + max (getHeight(bt_subleft(t))) (getHeight(bt_subright(t)))
;;

let rec imbalanceAvg_aux(t: 'a t_btree) : int =
  if bt_isempty(t)
  then 0
  else getHeight(bt_subleft(t)) - getHeight(bt_subright(t)) + imbalanceAvg_aux(bt_subleft(t)) + imbalanceAvg_aux(bt_subright(t))
;;

let imbalanceAvg(t: 'a t_btree) : float =
  if bt_isempty(t)
  then 0.0
  else (float_of_int(imbalanceAvg_aux(t))) /. (float_of_int(bt_size(t)))
;;

let avgImbalanceAvg(nbTree: int): float =
  let avg : float ref = ref 0.0 in
  for i = 1 to nbTree do
    avg := !avg +. imbalanceAvg(bst_rnd_create(25))
  done;
  let average : float = !avg/.(float_of_int(nbTree)) in
  Printf.printf "%f\n" average; 
  average
;;

(* QUESTION 3 MAIS CA MARCHE*)

let randList(sizel: int) : 'a list =
let l : 'a list ref = ref [] in
let rand : int ref = ref 0 in
  for i = 0 to sizel-1 do
    rand := !rand + Random.int(100) + 1;
    l := !l @ [!rand]
  done;
  !l
;;


let listRandomSize(nbList, listSizeMax : int * int) : 'a list =
  let mylist : 'a list ref = ref [] in
  for k = 1 to nbList do
    let newlist : 'a list = randList(listSizeMax) in
    mylist := !mylist @ newlist;
  done;
  !mylist 
;;

let listIncreasingSize(nbList : int) : 'a list =
  let mylist : 'a list ref = ref [] in
  for k = 1 to nbList do
    let newlist : 'a list = randList(k) in
    mylist := !mylist @ newlist;
  done;
  !mylist 
;;

let listDecreasingSize(nbList : int) : 'a list =
  let mylist : 'a list ref = ref [] in
  for k = 0 to nbList - 1 do
    let newlist : 'a list = randList(nbList - k) in
    mylist := !mylist @ newlist;
  done;
  !mylist 
;;

let listFixedSize(nbList, listSize : int * int) : 'a list =
  let mylist : 'a list ref = ref [] in
  for k = 1 to nbList do
    let newlist : 'a list = randList(listSize) in
    mylist := !mylist @ newlist;
  done;
  !mylist 
;;

let listToBst(l : 'a list) : 'a t_btree =
  let t : 'a t_btree ref = ref (bt_empty()) in
  let thelist : 'a list ref = ref l in
  while (!thelist <> []) do
    t := bst_linsert(!t, List.hd(!thelist));
    thelist := List.tl(!thelist);
  done;
  print(!t);
  !t
;;


let fixed : 'a list = listFixedSize(5, 4);;
let t : 'a t_btree = listToBst(fixed);;



let avgImbalanceList(nbrTest: int): unit =
  let avgRandSize: float ref = ref 0. in
  let avgIncrSize: float ref = ref 0. in
  let avgDecrSize: float ref = ref 0. in
  let avgFixeSize: float ref = ref 0. in
  let treeList: 'a t_btree ref = ref (bt_empty()) in
  for i = 1 to nbrTest do
    treeList := listToBst(listRandomSize(20, 10));
    avgRandSize := !avgRandSize +. imbalance(!treeList);

    treeList := listToBst(listIncreasingSize(20));
    avgIncrSize := !avgIncrSize +. imbalance(!treeList);

    treeList := listToBst(listDecreasingSize(20));
    avgDecrSize := !avgDecrSize +. imbalance(!treeList);

    treeList := listToBst(listFixedSize(20, 5));
    avgFixeSize := !avgFixeSize +. imbalance(!treeList)
  done;
  avgRandSize := !avgRandSize /. (float_of_int (nbrTest));
  Printf.printf "avgRandSize = %f\n"  !avgRandSize;
  
  avgIncrSize := !avgIncrSize /. float_of_int(nbrTest);
  Printf.printf "avgIncrSize = %f\n"  !avgIncrSize;
  
  avgDecrSize := !avgDecrSize /. float_of_int(nbrTest);
  Printf.printf "avgDecrSize = %f\n"  !avgDecrSize;
  
  avgFixeSize := !avgFixeSize /. float_of_int(nbrTest);
  Printf.printf "avgFixeSize = %f\n"  !avgFixeSize
;;