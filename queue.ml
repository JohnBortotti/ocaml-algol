type 'a cell = { content : 'a; mutable link : 'a cell }
type 'a queue = { mutable length : int; mutable head : 'a cell; mutable tail: 'a cell; }

exception Empty

let create () = { length = 0; head = Obj.magic None; tail= Obj.magic None; }

let push (x : int) (queue: 'a queue) =
  let rec cell = { content = x; link = cell } in
  match queue.length with
  | 0 ->
      queue.length <- 1;
      queue.head <- cell;
      queue.tail <- cell
  | x ->
      queue.length <- queue.length + 1;
      queue.tail.link <- cell;
      queue.tail <- cell

let pop (queue: 'a queue) = 
  match queue.length with
  | 0 -> raise Empty
  | x -> 
    queue.length <- queue.length - 1;
    queue.head.link <- queue.head.link;
    queue.head <- queue.head.link

let print_queue_top c =
  Printf.printf "head: %d \n" c.head.content;
  Printf.printf "head-link: %d \n" c.head.link.content;
  Printf.printf "tail: %d \n" c.tail.content

(* let take q = *)
let test =
  let queue = create () in
  push 20 queue;
  push 7 queue;
  push 10 queue;
  push 12 queue;
  pop queue;
  queue

let () = print_queue_top test
