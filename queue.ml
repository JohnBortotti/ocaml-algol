type 'a cell = { content : 'a; mutable link : 'a cell }
type 'a queue = { mutable length : int; mutable head : 'a cell }

exception Empty

let create () = { length = 0; head = Obj.magic None }

(* TODO: create cell *)
(* TODO: link with prev cell *)
let push x q = q.length <- 1

(* let take q = *)

let test = create ()
let () = Printf.printf "%d \n" 2
