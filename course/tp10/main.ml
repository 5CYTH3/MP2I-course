type 'a tree =
    | Vide
    | Noeud of 'a * 'a tree * 'a tree
;;

let empty_tree = Vide;;

let snd_tree = Noeud (1, Noeud (2, Noeud (4, Vide, Vide), Vide), Noeud (3, Vide, Vide));;

(* Question 2 *)
let rec height = function
    | Vide -> -1
    | Noeud (_, g, d) -> 1 + max (height g) (height d)
;;

let rec subtree_list a =
    a::match a with
    | Vide -> []
    | Noeud (_, g, d) -> subtree_list g @ subtree_list d
;;

let rec tree_max = function
    | Vide -> failwith "Empty tree provided"
    | Noeud (e, Vide, Vide) -> e
    | Noeud (e, Vide, f) | Noeud (e, f, Vide) -> max e (tree_max f)
    | Noeud (e, g, d) -> max e (max (tree_max g) (tree_max d))
;;

(* Question 3 *)
let is_leaf a = 
    match a with
    | Noeud (_, Vide, Vide) -> true
    | Vide | Noeud (_, _, _) -> false
;;

(* Question 4 *)
let rec leaf_count a =
    match a with
    | Noeud (e, g, d) when is_leaf (Noeud (e, g, d)) -> 1 + leaf_count g + leaf_count d
    | Vide | _ -> 0
;;

(* Question 5 *)
let rec label_sum = function
    | Vide -> 0
    | Noeud (e, g, d) -> e + label_sum g + label_sum d
;;

(* Question 6 *)
let rec leaf_sum a =
    match a with
    | Noeud (e, g, d) when is_leaf (Noeud (e, g, d)) -> e + leaf_sum g + leaf_sum d
    | Vide | _ -> 0
;;

(* Question 7 *)
let rec iter a =
    match a with
    | Vide -> []
    | Noeud (e, g, d) -> e::(iter g @ iter d)
;;

(* Question 8 *)
(* A finir *)
(*
let tree_min_max a =
    let iterator = iter a in
    let rec tree_max i m =
        match i with
        | [] -> failwith "empty"
        | h::n::t -> 
;;
*) 
(* Question 9 A faire *)

(****************
 * Exercice 2
 ****************)

(* Question 1 *)
type 'a tree_gen =
    | Vide
    | Noeud of 'a * 'a forest
and 'a forest = 'a tree_gen list;; 

(* CA MARCHE PAS MAIS JSP POURQUOI
let rec height a = 
    match a with
    | Vide -> -1
    | Noeud (_, l) -> 1 + forest_height l
and forest_height l =
    match l with 
    | [] -> -1
    | h::t -> (height h) (forest_height t)
;;
*)
