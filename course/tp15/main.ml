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

type 'a tree = ..

let get_tree i t = 

