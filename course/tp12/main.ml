(************** 
 * EXERCICE 1
 **************)

type 'a bst =
    | V
    | N of 'a bst * 'a * 'a bst
;;

let exemple_3 =
  N
    ( N (N (N (V, -6., V), -5.1, N (V, -4.8, V)), 1.2, N (V, 3.1, V)),
      3.5,
      N (N (V, 3.8, V), 5.1, N (V, 10.5, V)) )

let rec max_bst t =
    match t with
    | V -> failwith "Empty tree."
    | N (_, e, V) -> e
    | N (_, _, r) -> max_bst r
;;

let rec min_bst t = 
    match t with
    | V -> failwith "Empty tree."
    | N (V, e, _) -> e
    | N (l, _, _) ->  min_bst l 
;;

let rec is_bst t = 
    match t with
    | V -> true
    | N (V, e, V) -> true
    | N (l, e, V) -> max_bst l < e && is_bst l
    | N (V, e, r) -> min_bst r > e && is_bst r
    | N (l, e, r) ->
            max_bst l < e && min_bst r > e && is_bst l && is_bst r
;;

let rec bst_of_list l =
    match l with
    | [] -> V
    | h::t -> h (* C'est juste un placeholder, en gros on appelle récursivement bst_of_list que l'on donne à une variable et on insere h dans cette variable.*)
;;

(************** 
 * EXERCICE 2
 **************)

let rec delete t x =
    match t with
    | V -> failwith "Cannot delete an item that is not in the tree."
    | N (V, e, r) when e = x -> r
    | N (l, e, V) when e = x -> l
    | N (l, e, r) when e < x -> N (l, e, delete r x)
    | N (l, e, r) when e > x -> N (delete l x, e, r)
    | N (l, e, r) -> begin
        let rec fold l r =
            match r with
            | V -> N (l, e, V)

            | N (l', e', r') -> fold l' r'
        in delete (N (V, e, fold l r)) x
    end
;;

let _ = assert ((delete exemple_3 10.5) = N
    ( N (N (N (V, -6., V), -5.1, N (V, -4.8, V)), 1.2, N (V, 3.1, V)),
      3.5,
      N (N (V, 3.8, V), 5.1, V) ));;

let _ = assert((delete exemple_3 (-5.1)) = N
    ( N (N (N (V, -6., V), -4.8, V), 1.2, N (V, 3.1, V)),
      3.5,
      N (N (V, 3.8, V), 5.1, N (V, 10.5, V)) ));; 

(* Question 4 *)
let rec delete_mkii t x =
    let rec get_min t = 
        match t with 
        | V -> failwith "vide"
        | N (V, e, r) -> (e, r)
        | N (l, e, r) -> 
                let (min, remainder) = get_min l in (min, N (remainder, e, r))
        in
    match t with
    | V -> failwith "etiquette absente"
    | N (V, e, r) when e = x -> r
    | N (l, e, V) when e = x -> l
    | N (l, e, r) when e < x -> N (l, e, delete_mkii r x)
    | N (l, e, r) when e > x -> N (delete_mkii l x, e, r)
    | N (l, e, r) -> let (x, y) = get_min r in N (l, x, y)
;; 

