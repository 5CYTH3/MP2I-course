let exemple =
    [ "sire"; "site"; "ski"; "sac"; "dodos"; "dodu"; "dole"; "de"; "si"; "do" ]
;;

type trie =
    | V 
    | N of char * trie * trie
;;

type mot = char list;;

(**************)
(* Exercice 1 *)
(**************)

(* Question 1 *)
let rec est_bien_forme t =
    match t with
    | V -> true
    | N ('$', g, d) -> g = V && est_bien_forme d
    | N (_, g, d) -> est_bien_forme g && est_bien_forme d
;;

(* Question 2 *)
let mot_of_string s = 
    let n = String.length s - 1 in
    (* aux : int -> mot -> mot 
     * Prend en entrée un compteur [k] et une liste [l]. * À chaque itération, si le caractère à l'indice `k` dans la string s n'est pas le caractère `$`, passe à la prochaine
     * itération en incrémentant le compteur et en ajoutant à la liste l'élément à l'indice n-k avec n la taille de la string - 1
     * (car on ajoute les éléments en tête). Sinon, on renvoie la liste avec le dernier caractère ajouté en tête.
     *)
    let rec aux k l =
        if s.[k] = '$' then s.[n - k]::l
        else aux (k + 1) (s.[n - k]::l)
    in aux 0 []
;;

let _ = assert (mot_of_string "yeah$" = ['y'; 'e'; 'a'; 'h'; '$']);;

(* Question 3 *)
let rec afficher_mot (m: mot) = 
    match m with
    | [] | ['$'] -> print_endline ""
    | h::t -> print_char h; afficher_mot t
;;

let _ = "yeah$" |> mot_of_string |> afficher_mot

(**************)
(* Exercice 2 *)
(**************)

let exemple = [
    "sire";
    "site";
    "ski";
    "sac";
    "dodos";
    "dodu";
    "dole";
    "de";
    "si";
    "do";
];;

let trie_exemple =
  N
    ( 'd',
      N
        ( 'o',
          N
            ( '$',
              V,
              N
                ( 'l',
                  N ('e', N ('$', V, V), V),
                  N
                    ( 'd',
                      N
                        ( 'u',
                          N ('$', V, V),
                          N ('o', N ('s', N ('$', V, V), V), V) ),
                      V ) ) ),
          N ('e', N ('$', V, V), V) ),
      N
        ( 's',
          N
            ( 'i',
              N
                ( '$',
                  V,
                  N
                    ( 't',
                      N ('e', N ('$', V, V), V),
                      N ('r', N ('e', N ('$', V, V), V), V) ) ),
              N
                ( 'a',
                  N ('c', N ('$', V, V), V),
                  N ('k', N ('i', N ('$', V, V), V), V) ) ),
          V ) 
        )

(* Question 1 *)
let rec cardinal t =
    match t with
    | V -> 0
    | N ('$', g, d) -> 1 + cardinal g + cardinal d
    | N (_, g, d) -> cardinal g + cardinal d 
;;

let _ = assert (cardinal trie_exemple = 10);;

(* Question 2 *)
let recherche t m =
    (* [rev_m] représente le mot m à l'envers. Ce constant est utile
     * lors de la comparaison ligne 127, sachant que les éléments sont 
     * scannés de haut en bas, mais sont ajoutés en tête de liste.
     *)
    let rev_m = List.rev m in
    (* are_eq : mot -> mot -> bool
     * Vérifie que les mots [m1] et [m2] sont identiques.
     *)
    let rec are_eq m1 m2 =
        match (m1, m2) with
        | [], [] -> true
        | _, [] | [], _ -> false
        | (h::t, h'::t') -> h = h' && are_eq t t'
    in 
    (* sub_recherche : trie -> mot -> bool
     * Scanne les éléments du trie [t] de haut en bas, et ajoute dans un accumulateur 
     * représentant le mot en construction [m'] le caractère du noeud courant s'il n'est pas '$'.
     * Si c'est le cas, compare [m'] et [rev_m]. 
     *)
    let rec sub_recherche t m' =
        match t with
        | V -> false
        | N ('$', V, d) -> are_eq rev_m ('$'::m') || sub_recherche d m'
        | N (c, g, d) -> sub_recherche g (c::m') || sub_recherche d m'
    in
    sub_recherche t []
;;

let _ = assert (recherche trie_exemple ['s'; 'i'; 't'; 'e'; '$'] = true)

(* Question 3 *)
let rec insere t m = 
    match (t, m) with
    | _, [] -> t
    | V, h::t -> N (h, insere V t, V)
    | N (c, g, d), h::t ->
            if c = h then N (c, insere g t, d)
            else if h < c then N (c, insere g m, d)
            else N (c, g, insere d m)
;;

let _ = assert (insere trie_exemple ['s'; 'i'; 't'; 'e'; '$'] = trie_exemple);;


