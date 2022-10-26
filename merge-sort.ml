let input = [5; 2; 10; 1; 7; 22; 4; 20; 7]

exception Empty

let rec split = function
  | []
  | [_] as t1 -> t1, []
  | hd :: tl->
      let t1, t2 = split tl in
      hd::t2, t1

let rec merge = function
   |list, []
   | [], list -> list
   | hd1::tl1, hd2::tl2 ->
      if hd1 <= hd2 then
         hd1::merge(tl1, hd2::tl2)
      else
         hd2::merge(hd1::tl1, tl2)

let rec merge_sort = function
   | []
   | [_] as list -> list
   | list ->
      let l1, l2 = split list in
      merge(merge_sort l1, merge_sort l2)


let test = merge_sort input

let () = List.iter (Printf.printf "%d \n") test