type bit = Z | U;;
type number = bit list;;

let zero = [];;
let siz = [Z; U; U];;

let rec succ (num: number) =
    match num with
    | [] | [Z] -> [U]
    | h::t -> 
            if h == U then
                h::(succ t)
            else U::t
;;

let rec pred num = 
    match num with
    | [] | [U] -> [Z]
    | h::t -> if h == Z then h::(pred t) else Z::t
;;

(* Ici, la fonction succ a une complexité de O(n)*)

type 'a tree = 
    | F of 'a 
    | N of int * 'a tree * 'a tree
;;

let rec get_tree i t = 
    match t with
    | F k -> k
    | N (k, l, v) ->
            if i > k then failwith "Index out of range"
            else if i < k / 2 then get_tree i l
            else get_tree (i - k / 2) v
;;


let get l i =
    let rec aux l i n = 
        match l with
        | [] -> failwith "Out of range"
        | None::t -> aux t i (n * 2)
        | Some v :: t -> if i >= n then aux t (i - n) (n * 2)
            else get_tree i v
    in aux l i 1
;;

let rec set_tree i x t =
    match t with
    | F e when i = 0 -> F x
    | N (e, g, d) when i < e ->
            if i < e / 2 then N (e, set_tree i x g, d)
            else N (e, g, set_tree (i-e/2) x d)
    | _ -> t
;;

let size t = 
    match t with
    | N (t, _, _) -> t
    | F _ -> 1
;;

let cons x l = 
    let rec aux l a =
        match l with
        | None::t -> Some a :: t
        | Some v::t -> None::aux t (N (2 * size a, a, v))
        | [] -> [Some a] 
    in
    aux l (F x)
;;
