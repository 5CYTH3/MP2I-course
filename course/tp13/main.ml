type couleur = Rouge | Noir

type 'a arn = V | N of couleur * 'a arn * 'a * 'a arn

let exemple =
  N
    ( Noir,
      N
        ( Noir,
          N
            ( Rouge,
              N (Noir, N (Rouge, V, -27, V), -15, V),
              -5,
              N (Noir, V, -4, V) ),
          -3,
          N (Noir, N (Rouge, V, -2, V), -1, V) ),
      1,
      N
        ( Noir,
          N (Noir, V, 7, V),
          12,
          N (Noir, N (Rouge, V, 15, V), 18, V) ) )

(*
 * Exercice 1
 *)
let rec node_color t =
    match t with
    | V -> Noir
    | N (c, _, _, _) -> c
;;

let rec max_rbt t =
    match t with
    | V -> failwith "No maximum in an empty rbt"
    | N (_, _, e, V) -> e
    | N (_, _, _, r) -> max_rbt r
;;

let rec min_rbt t = 
    match t with
    | V -> failwith "No minimum in an empty rbt"
    | N (_, V, e, _) -> e
    | N (_, l, _, _) -> min_rbt l
;;

let rec find t x =
    match t with
    | V -> failwith "Not in the tree"
    | N (c, l, e, r) -> 
            if x = e then true
            else if x < e then find l x
            else find r x 
;;

let rec black_height t =
    match t with
    | V -> 0
    | N (c, l, e, r) -> (if c = Noir then 1 else 0) + max (black_height l) (black_height r)
;;

let rec is_correct_sub_rbt t = 
    match t with
    | V -> true
    | N (Rouge, N (Rouge, _, _, _), _, _)
    | N (Rouge, _, _, N (Rouge, _, _, _)) -> false
    | N (_, l, _, r) ->
            black_height l = black_height r && is_correct_sub_rbt l && is_correct_sub_rbt r
;;

let remonte_rouge t =
    match t with
    | N (Noir, N (Rouge, N (Rouge, a1, x, a2), y, a3), z, a4) 
    | N (Noir, N (Rouge, a1, x, N (Rouge, a2, y, a3)), z, a4)
    | N (Noir, a1, x, N (Rouge, N (Rouge, a2, y, a3), z, a4))
    | N (Noir, a1, x, N (Rouge, a2, y, N (Rouge, a3, z, a4)))
    -> N (Rouge, N (Noir, a1, x, a2), y, N (Noir, a3, z, a4)) 
    | _ -> t 
;;

let rec insert_aux t x =
    assert(is_correct_sub_rbt t);
    match t with
    | V -> N (Rouge, V, x, V)
    | N (c, l, e, r) -> 
            if x < e then remonte_rouge (N (c, insert_aux l x, e, r))
            else if x > e then remonte_rouge (N (c, l, e, insert_aux r x))
            else t
;;

let rec insert t x = 
    match insert_aux t x with
    | V -> failwith "error in the process"
    | N (_, g, e, d) -> N (Noir, g, e, d)
;;

(* Sachant insert_aux en O(n) avec n = h(A) alors insert est en O(n) *)

(**************)
(* Exercice 3 *)
(**************)

(* Question 1 *)
let rec delete_min = function
    | V -> V, false
    | N (Rouge, V, e, d) -> d, false
    | N (Noir, V, e, d) -> d, true
    | 
