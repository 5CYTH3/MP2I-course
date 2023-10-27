(** Enogad LE BIAVANT--FRÉDÉRIC *)
(** ça aurait été plus simple de faire la documentation dans un .mli *)

let rec puissance x n = 
    if n = 0 then 1 
    else if n < 0 then 
        failwith "Impossible de mettre un entier relatif à la puissance" 
    else 
        (puissance x (n - 1)) * x
;;

let _ = assert (puissance 2 4 = 16);;

let rec somme_n_puissance_k n k = 
    if n = 0 then 0 
    else 
        puissance n k + somme_n_puissance_k (n - 1) k
;;

let _ = assert (somme_n_puissance_k 2 5 = 33);;

let rec binom k n = 
    if k = n || k = 0 || n = 0 then 1 
    else 
        binom (k - 1) (n - 1) + binom k (n - 1)
;;

let _ = assert (binom 2 10 = 45);;
let _ = assert (binom 1 4 = 4);;
let _ = assert (binom 5 5 = 1);;

let rec croissant l = 
    match l with
    | [] -> true
    | _::[] -> true
    | h::n::t -> if h <= n then croissant (n::t) else false
;;

let _ = assert (croissant [2; 3; 4; 1] = false);;
let _ = assert (croissant [1; 5; 6; 6] = true);;
let _ = assert (croissant [2; 1; 2; 3] = false);;

let rec compare_tailles l1 l2 = 
    match (l1, l2) with
    | ([], []) -> 0
    | (_, []) -> -1
    | ([], _) -> 1
    | (_::t1,_::t2) -> compare_tailles t1 t2
;;

let _ = assert (compare_tailles [2; 3; 4; 1; 6] [3; 2; 5] = -1);;
let _ = assert (compare_tailles [2; 3; 4] [3; 2; 5] = 0);;
let _ = assert (compare_tailles [2; 3; 4] [3; 2; 5; 4] = 1);;

(* Exercice 2 *)

(* insere : 'a list -> 'a -> 'a list
 * [insere l x] puis renvoie une permutation de [l] une liste ordonnée contenant [x], où l'ordre est préservé.
 *)
let rec insere l x =
    match l with
    | [] -> [x]
    | h::t -> if h >= x then x::l else h::(insere t x)
;;

let _ = assert (insere [2; 3; 6; 7] 4 = [2; 3; 4; 6; 7]);;
let _ = assert (insere [6; 6; 7] 4 = [4; 6; 6; 7]);;
let _ = assert (insere [1; 2; 3; 4] 2 = [1; 2; 2; 3; 4]);;

(* tri_insertion : 'a list -> 'a list
 * [tri_insertion l] renvoie une permutation ordonnée de [l].
 *)
let rec tri_insertion l =
    match l with
    | [] -> []
    | h::t -> insere (tri_insertion t) h
;;

let _ = assert (tri_insertion [1; 5; 2; 4] = [1; 2; 4; 5]);;

(* Exercice 3 *)

(* partitionne : 'a list -> ('a list * 'a * 'a list)
 * [partitionne l] prend en entrée une liste non vide et renvoie un triplet contenant 
 * la liste des éléments inférieurs où égaux à la tête de [l], celle des éléments strictement 
 * supérieurs et enfin la tête elle-même.
 *)
let partitionne l =
    (* partitionne_aux : 'a list -> 'a -> ('a list * 'a * 'a list)
     * [partitionne_aux lx p] prend une liste [lx] et un pivot [p] en entrée puis retourne un triplet
     * contenant la list des éléments inférieurs au pivot, celle des éléments supérieurs et le pivot lui-même.
     *)
    let rec partitionne_aux lx p =
        match lx with
        | [] -> ([], p, [])
        | h::t -> let (l1, pv, l2) = partitionne_aux t p in
            if h <= p then
                (h::l1, p, l2)
            else
                (l1, p, h::l2)
    in 
    match l with
    | [] -> failwith "no"
    | h::t -> partitionne_aux t h

;;

let _ = assert (partitionne [3; 6; 3; 1; 7; 2; 8] = ([3; 1; 2], 3, [6; 7; 8]));;


(* tri_rapide : 'a list -> 'a list
 * [tri_rapide l] prend en entrée une liste [l] et renvoie une permutation ordonnée de [l].
 *)
let rec tri_rapide l = 
    match l with
    | [] -> []
    | h::t -> let (l1, p, l2) = partitionne l in (tri_rapide l1) @ (p::(tri_rapide l2))
;;

let _ = assert (tri_rapide [2; 3; 1; 6; 4] = [1; 2; 3; 4; 6]);;

(* Exercice 4 *)
type monome = {
    coeff: int;
    degre: int;
};;

type polynome = monome list;;

let poly_q1: polynome = [
    {coeff = -1; degre = 10};
    {coeff = 3; degre = 2};
    {coeff = 2; degre = 1};
];;

let rec polynome_valide p = 
    match p with
        | [] -> true
        | h::[] -> h.coeff <> 0 && h.degre >= 0
        | h::n::t -> h.coeff <> 0 && (h.degre > n.degre) && polynome_valide (n::t)
;; 

let _ = assert (polynome_valide poly_q1 = true);;
let _ = assert (polynome_valide [{ coeff = 1; degre = 1}] = true);;
let _ = assert (polynome_valide [{ coeff = 1; degre = 1}; { coeff = 2; degre = 2}] = false);;

let rec degre p = 
    match p with
    | [] -> -1
    | h::_ -> h.degre
;;

let rec puissance_entiere x n =
    if n = 0 then 1 else
        if n mod 2 = 0 then
            puissance_entiere (x * x) (n / 2)
        else
            x * puissance_entiere (x * x) (n /2)
;;

let _ = assert (puissance_entiere 3 2 = 9);;
let _ = assert (puissance_entiere 2 4 = 16);;

let rec evaluation p x =
    match p with
    | [] -> 0
    | h::t -> (h.coeff * puissance_entiere x h.degre) + evaluation t x
;;

let _ = assert (evaluation poly_q1 1 = 4);;

let rec composition_carre p =
    match p with
    | [] -> []
    | h::t -> { coeff = h.coeff; degre = h.degre * 2}::(composition_carre t)
;;

let _ = assert (composition_carre poly_q1 = [{ coeff = -1; degre = 20 }; { coeff = 3; degre = 4 }; { coeff = 2; degre = 2 }]);;

let rec composition p n =
    match p with
    | [] -> []
    | h::t -> { coeff = h.coeff; degre = h.degre * n }::(composition t n)
;;

let _ = assert (composition poly_q1 2 = [{ coeff = -1; degre = 20 }; { coeff = 3; degre = 4 }; { coeff = 2; degre = 2 }]);;

let rec somme p1 p2 =
    match (p1, p2) with
    | ([], _) -> p2
    | (_, []) -> p1
    | (h1::t1, h2::t2) when h1.degre = h2.degre -> { coeff = h1.coeff + h2.coeff; degre = h1.degre }::(somme t1 t2)
    | (h1::t1, h2::t2) -> if h1.degre > h2.degre then h1::h2::(somme t1 t2) else h2::h1::(somme t1 t2)
;;

let _ = assert (somme poly_q1 [{ coeff = 2; degre = 10 }; { coeff = 4; degre = 4 }] = [
    { coeff = 1; degre = 10 }; 
    {coeff = 4; degre = 4 }; 
    { coeff = 3; degre = 2 }; 
    { coeff = 2; degre = 1 }; 
]);;

let rec derivee p =
    match p with
    | [] -> []
    | h::t -> let { coeff = co; degre = deg } = h in
    { coeff = co * deg; degre = deg - 1 }::(derivee t)
;;

let _ = assert (derivee poly_q1 = [
    { coeff = -10; degre = 9 };
    { coeff = 6; degre = 1 };
    { coeff = 2; degre = 0 };
]);;

let rec produit p1 p2 = 
    let rec produit_aux m p =
        match p with
        | [] -> []
        | h::t -> { coeff = m.coeff * h.coeff; degre = m.degre + h.degre }::(produit_aux m t)
    in
    match p1 with
    | [] -> []
    | h::t -> somme (produit_aux h p2) (produit t p2)
;;

let _ = assert (produit poly_q1 [{ coeff = 2; degre = 10 }; { coeff = 4; degre = 4 }] = [
    { coeff = -2; degre = 20 }; 
    { coeff = 6; degre = 12 }; 
    { coeff = -4; degre = 14 };
    { coeff = 4; degre = 11 }; 
    { coeff = 12; degre = 6 }; 
    { coeff = 8; degre = 5 }
]);
