(* Selection Sort *)

(* return the min element and the list *)
let rec select x = function
  | [] -> (x, [])
  | h :: t ->
      let x, h = if h > x then (x, h) else (h, x) in
      let x, t = select x t in
      (x, h :: t)

(* sort recursive *)
let rec sort = function
  | [] -> []
  | h :: t -> ( match select h t with h, t -> h :: sort t)

let res = sort [ 9; 3; 2; 14; 26 ]
let () = List.iter (Printf.printf "%d \n") res