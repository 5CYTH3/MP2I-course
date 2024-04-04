type formula = 
    | Prop of int 
    | Neg of formula
    | And of formula * formula
    | Or of formula * formula
    | Impl of formula * formula
    | Top
    | Bottom
;;

type value = bool array;;

(* Question 2 *)
let exemple = Impl (
    Or (Bottom, Prop 0),
    And (
        And (
            Neg (Prop 1),
            Or (
                Prop 2,
                Prop 0
            )
        ),
        Impl (
            Top,
            Or (
                Prop 2,
                Prop 3
            )
        )
    )
)

(* Question 3 *)
let rec length f = 
    match f with
    | Prop _ | Top | Bottom -> 0
    | And (a, b) | Or (a, b) | Impl (a, b) -> 1 + length a + length b 
    | Neg a -> 1 + length a 
;;

(* Question 4 *)
let ind_var_max f = 
    let max_ind = ref (-1) in
    let rec aux f =
        match f with
        | Prop a -> if a > !max_ind then max_ind := a
        | Neg a -> aux a 
        | Or (a, b) | And (a, b) | Impl (a, b) -> aux a; aux b
        | _ -> ()
    in aux f;
    !max_ind
;;

(* Question 5 *)
let rec truth_value f v =
    match f with
    | Top -> true
    | Bottom -> false
    | Prop x -> v.(x)
    | Neg x -> truth_value x v
    | Or (a, b) -> truth_value a v || truth_value b v
    | And (a, b) -> truth_value a v && truth_value b v
    | Impl (a, b) -> truth_value (Neg a) v || truth_value b v
;;

(* Question 6 *)
(*
 * Pour [n] la taille de l'entrée f une [formula]
 * On a alors O(n)
 *)

(**************)
(* Exercice 2 *)
(**************)

exception Debordement;;

(* Question 1 *) 
let increment_valuation v = 
    let n = Array.length v in 
    let rec aux k =
        if k = -1 then raise Debordement;
        (* Dans le premier cas il faut mettre tout ce qu'il y a avant à 0 *)
        if v.(k) = false then v.(k) <- true else begin 
            v.(k) <- false; aux (k-1) 
        end
    in aux (n-1)
;;

(* Question 2 *)
let raw_sat f = 
    let v = Array.make (ind_var_max f + 1) false in
    let rec aux f =
        if truth_value f v then true
        else try (increment_valuation v; aux f) with Debordement -> false
    in aux f
;;

(* Question 3 *)
let equiv f1 f2 =
    let v = Array.make (max (ind_var_max f1) (ind_var_max f2)) false in
    let state = ref true in
    let rec aux f1 f2 =
        if truth_value f1 v && truth_value f2 v then (increment_valuation v; aux f1 f2) else state := false 
    in aux f1 f2;
    !state
;;

(* Question 4 *)
let rec del_impl f =
    match f with
    | Prop x -> Prop x
    | Neg x -> Neg (del_impl x)
    | Top -> Top
    | Bottom -> Bottom
    | Or (a, b) -> Or (del_impl a, del_impl b)
    | And (a, b) -> And (del_impl a, del_impl b)
    | Impl (a, b) -> Or (Neg (del_impl a), del_impl b) 
;;

let rec push_neg f =
    match f with
    | Prop x -> Prop x
    | Neg x -> begin match x with
        | Prop x -> Neg (Prop x)
        | Neg x -> x
        | And (a, b) -> Or (push_neg (Neg a), push_neg (Neg b))
        | Or (a, b) -> And (push_neg (Neg a), push_neg (Neg b))
        | Impl (a, b) -> push_neg (Neg (del_impl (Impl (a, b))))
        | Bottom -> Top
        | Top -> Bottom
    end
    | And (a, b) -> And (push_neg a, push_neg b)
    | Or (a, b) -> Or (push_neg a, push_neg b)
    | Impl (a, b) -> Impl (push_neg a, push_neg b) 
    | x -> x 
;;

(*
let rec fnc f =
    match f with
    | 
*)
(**************)
(* Exercice 3 *)
(**************)

(* Question 1 *)
(* \phi \impl T \equiv T *)
(* \phi \impl _ \equiv \not \phi *)
(* T \impl \phi \equiv \phi *)
(* _ \impl \phi \equiv T *)

(* Question 2 *)
(* Bon ici je crois que j'en ai plus fait que prévu... cf photos*)
let rec simplifier_constantes f =
    match f with
    | Prop p -> Prop p
    | Neg Bottom -> Top
    | Neg Top -> Bottom
    | Neg x -> Neg x 
    | Bottom -> Bottom
    | Top -> Top
    | Or (_, Top) | Or (Top, _) -> Top
    | Or (phi, Bottom) | Or (Bottom, phi) -> simplifier_constantes phi
    | And (Top, phi) | And (phi, Top) -> simplifier_constantes phi
    | And (Bottom, _) | And (_, Bottom) -> Bottom
    | Impl (_, Top) | Impl (Bottom, _) -> Top
    | Impl (phi, Bottom) -> simplifier_constantes (Neg phi)
    | Impl (Top, phi) -> simplifier_constantes phi
    (* Now is the "normal workflow" *)
    | Or (a, b) -> simplifier_constantes (Or (simplifier_constantes a, simplifier_constantes b))
    | And (a, b) -> simplifier_constantes (And (simplifier_constantes a, simplifier_constantes b)) 
    | Impl (a, b) -> simplifier_constantes (Impl (simplifier_constantes a, simplifier_constantes b))
;;

let rec substitution phi k psi =
    match phi with
    | Prop k' when k' = k -> psi
    | Prop _ | Top | Bottom -> phi
    | Neg phi' -> Neg (substitution phi' k psi)
    | And (phi1, phi2) -> And (substitution phi1 k psi, substitution phi2 k psi)
    | Or (...)
    | Impl (...)
;;

let rec sub_and_simpl phi k psi = 
    match phi with
    | Prop k' when k = k' -> psi
    | Prop _ | Top | Bottom -> phi
    | Neg phi' -> simplifier_constantes (Neg (sub_and_simpl phi'k psi))
    | ...
;;

let rec build_decision_tree phi = 
    match phi with
    | Top -> Leaf true
    | Bottom -> Leaf false
    | _ -> let k = ind_var_max phi in
            let phi_B = sub_and_simpl phi k Bottom in
            let phi_T = sub_and_simpl phi k Top in
            Node (build_decision_tree phi_T, k, build_decision_tree phi_B)
;;

let satisfying_quine phi = 
    let a = build_decision_tree phi in
    let rec aux = function
        | Leaf b -> b
        | Node (g, _, d) -> aux g || aux d
    in aux a
;;
        
