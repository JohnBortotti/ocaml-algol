module LinkedList = struct
  type 'a node = { content : 'a; mutable link : 'a node }

  let create_node content = { content; link = Obj.magic None }
  let push (x : 'a node) (y : 'a node) = x.link <- y

  exception Empty
end

module Graph = struct
  type 'a graph = { mutable g : 'a LinkedList.node list }

  let create = { g = [] }
  let push x g = g.g <- x :: g.g
  let iter (f : 'a -> unit) graph = List.iter f graph.g

  (* get node chain *)
  (* check if the element is present in the graph *)
  (* graph traversal *)
  (* finding the path from one vertex to another *)
  exception Empty
end

let () =
  let node1 = LinkedList.create_node 1 in
  let node2 = LinkedList.create_node 2 in
  let node3 = LinkedList.create_node 3 in
  let graph = Graph.create in
  LinkedList.push node1 node2;
  LinkedList.push node2 node3;
  LinkedList.push node3 node1;
  Graph.push node1 graph;
  Graph.push node2 graph;
  Graph.iter (fun x -> Printf.printf "%d \n" x.link.content) graph
