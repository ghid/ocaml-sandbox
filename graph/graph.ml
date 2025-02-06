(* type graph = { edges : (int, int list) Hashtbl.t } *)
type 'a graph = { edges : ('a, 'a list) Hashtbl.t }

let dfs_traverse ~g ~start =
  let to_visit = Stack.create () in
  let visited = Hashtbl.create 16 in
  Stack.push start to_visit;
  let rec loop visited_nodes =
    if not (Stack.is_empty to_visit)
    then (
      let node = Stack.pop to_visit in
      explore_node visited_nodes node)
    else List.rev visited_nodes
  and explore_node visted_nodes node =
    if not (Hashtbl.mem visited node)
    then (
      Hashtbl.add visited node ();
      let children = Hashtbl.find_opt g.edges node |> Option.value ~default:[] in
      List.iter
        (fun child ->
           match Hashtbl.find_opt visited child with
           | None -> Stack.push child to_visit
           | Some _ -> ())
        children;
      loop (node :: visted_nodes))
    else loop visted_nodes
  in
  loop []
;;

let bfs_traverse ~g ~start =
  let to_visit = Queue.create () in
  let visited = Hashtbl.create 16 in
  Queue.push start to_visit;
  let rec loop visited_nodes =
    if not (Queue.is_empty to_visit)
    then (
      let node = Queue.pop to_visit in
      explore_node visited_nodes node)
    else List.rev visited_nodes
  and explore_node visted_nodes node =
    if not (Hashtbl.mem visited node)
    then (
      Hashtbl.add visited node ();
      let children = Hashtbl.find_opt g.edges node |> Option.value ~default:[] in
      List.iter (fun child -> Queue.push child to_visit) children;
      loop (node :: visted_nodes))
    else loop visted_nodes
  in
  loop []
;;

(** Search in graph [g] using BFS, starting from node [start].
   It returns the first node that satisfies [p] or [None] if
   no node reachable from [start] satisfies [p]. *)
let search_for ~g ~start (p : 'a -> bool) : 'a option =
  let to_visit = Queue.create () in
  let visited = Hashtbl.create 16 in
  Queue.push start to_visit;
  let rec loop () =
    if Queue.is_empty to_visit
    then None
    else (
      (* node to explore *)
      let node = Queue.pop to_visit in
      explore_node node)
  and explore_node node =
    if not (Hashtbl.mem visited node)
    then
      if p node
      then Some node (* found *)
      else (
        Hashtbl.add visited node ();
        let children = Hashtbl.find_opt g.edges node |> Option.value ~default:[] in
        List.iter (fun child -> Queue.push child to_visit) children;
        loop ())
    else loop ()
  in
  loop ()
;;

(* A sample graph *)
let my_graph =
  let edges =
    List.to_seq
      [ 1, [ 2; 3 ]
      ; 2, [ 10; 11 ]
      ; 3, [ 4; 5 ]
      ; 5, [ 12; 100 ]
      ; 11, [ 0; 20 ]
      ; 12, [ 13; 14 ]
      ]
    |> Hashtbl.of_seq
  in
  { edges }
;;

let my_graph2 =
  let edges =
    List.to_seq
      [ 0, [ 1; 2 ]
      ; 1, [ 2; 3; 4 ]
      ; 2, [ 0; 1 ]
      ; 3, [ 1; 5 ]
      ; 4, [ 1 ]
      ; 5, [ 3; 6; 7; 8 ]
      ; 6, [ 5 ]
      ; 7, [ 5; 8 ]
      ; 8, [ 5; 7; 9 ]
      ; 9, [ 8 ]
      ]
    |> Hashtbl.of_seq
  in
  { edges }
;;

let char_graph =
  let edges =
    List.to_seq
      [ 'b', [ 'c'; 'f' ]
      ; 'c', [ 'b'; 'f' ]
      ; 'd', []
      ; 'f', [ 'b'; 'c'; 'k' ]
      ; 'g', [ 'h' ]
      ; 'k', [ 'f' ]
      ]
    |> Hashtbl.of_seq
  in
  { edges }
;;

let jenny's_graph =
  let edges =
    List.to_seq
      [ 0, [ 1; 3 ]
      ; 1, [ 0; 3; 2; 5; 6 ]
      ; 2, [ 3; 1; 5; 4 ]
      ; 3, [ 0; 1; 2; 4 ]
      ; 4, [ 3; 2; 6 ]
      ; 5, [ 1; 2 ]
      ; 6, [ 4; 1 ]
      ]
    |> Hashtbl.of_seq
  in
  { edges }
;;
