type 'a stack = { mutable s : 'a list }

exception Empty

let create () = { s = [] }

let push x s = s.s <- x :: s.s

let pop s =
  match s.s with
  | [] -> raise Empty
  | hd :: tl ->
      s.s <- tl;
      hd

let test =
  let stack = create () in
  push 1 stack;
  push 2 stack;
  stack

let () = List.iter (Printf.printf "%d \n") test.s