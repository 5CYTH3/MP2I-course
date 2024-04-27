(*
let max_contiguous_sum_memoized t =
    let cache = Hashtbl.create (Array.length t + 1) in
    let tab = Array.init (Array.length t) (fun i -> f t cache i) in
    max_array tab
;;
*)

let max_somme_contigue t =

