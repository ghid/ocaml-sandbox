open Tree

(* Benchmarking *)
let n_iter = 50_000_000

let time f () =
  let start = Sys.time () in
  f ();
  let elapsed = Sys.time () -. start in
  elapsed
;;

(* top *)
let benchmark_top_tree_1 () =
  let t = Tree (Empty, 2, Empty) in
  for _ = 1 to n_iter do
    assert (top t = Some 2)
  done
;;

let benchmark_top_gtree_1 () =
  let t = TreeG (EmptyG, 2, EmptyG) in
  for _ = 1 to n_iter do
    assert (topG t = 2)
  done
;;

let benchmark_top_tree_2 () =
  let t = Tree (Empty, 2, Empty) in
  for _ = 1 to n_iter do
    assert (top (incr (incr (incr (incr t)))) = Some 6)
  done
;;

let benchmark_top_gtree_2 () =
  let t = TreeG (EmptyG, 2, EmptyG) in
  for _ = 1 to n_iter do
    assert (topG (incrG (incrG (incrG (incrG t)))) = 6)
  done
;;

let benchmark_top_tree_3 () =
  let t = Tree (Tree (Empty, 3, Empty), 2, Tree (Empty, 4, Empty)) in
  for _ = 1 to n_iter do
    assert (depth t = 2)
  done
;;

let benchmark_top_gtree_3 () =
  let t = TreeG (TreeG (EmptyG, 3, EmptyG), 2, TreeG (EmptyG, 4, EmptyG)) in
  let two = S (S Z) in
  (* let t' =
    TreeG (TreeG (TreeG (EmptyG, 1, EmptyG), 3, EmptyG), 2, TreeG (EmptyG, 4, EmptyG))
  in
  let three = S (S (S Z)) in *)
  for _ = 1 to n_iter do
    assert (depthG t = two)
  done
;;

let benchmark_top_tree_4 () =
  let t = Tree (Tree (Empty, 3, Empty), 2, Tree (Empty, 4, Empty)) in
  let t2 = Tree (t, 5, t) in
  let t3 = Tree (t2, 5, t2) in
  let t4 = Tree (t3, 5, t3) in
  let t5 = Tree (t4, 5, t4) in
  let t6 = Tree (t5, 5, t5) in
  let t7 = Tree (t6, 100, t6) in
  for _ = 1 to n_iter do
    assert (top (incr (incr (incr (incr t7)))) = Some 104)
  done
;;

let benchmark_top_gtree_4 () =
  let t = TreeG (TreeG (EmptyG, 3, EmptyG), 2, TreeG (EmptyG, 4, EmptyG)) in
  let t2 = TreeG (t, 5, t) in
  let t3 = TreeG (t2, 5, t2) in
  let t4 = TreeG (t3, 5, t3) in
  let t5 = TreeG (t4, 5, t4) in
  let t6 = TreeG (t5, 5, t5) in
  let t7 = TreeG (t6, 100, t6) in
  for _ = 1 to n_iter do
    assert (topG (incrG (incrG (incrG (incrG t7)))) = 104)
  done
;;

let benchmark_top_tree_5 () =
  let t = Tree (Tree (Empty, 3, Empty), 2, Tree (Empty, 4, Empty)) in
  let t2 = Tree (t, 5, t) in
  let t3 = Tree (t2, 5, t2) in
  let t4 = Tree (t3, 5, t3) in
  let t5 = Tree (t4, 5, t4) in
  let t6 = Tree (t5, 5, t5) in
  let t7 = Tree (t6, 100, t6) in
  for _ = 1 to n_iter / 100 do
    assert (top (swivel (swivel (swivel (swivel t7)))) = Some 100)
  done
;;

let benchmark_top_gtree_5 () =
  let t = TreeG (TreeG (EmptyG, 3, EmptyG), 2, TreeG (EmptyG, 4, EmptyG)) in
  let t2 = TreeG (t, 5, t) in
  let t3 = TreeG (t2, 5, t2) in
  let t4 = TreeG (t3, 5, t3) in
  let t5 = TreeG (t4, 5, t4) in
  let t6 = TreeG (t5, 5, t5) in
  let t7 = TreeG (t6, 100, t6) in
  for _ = 1 to n_iter / 100 do
    assert (topG (swivelG (swivelG (swivelG (swivelG t7)))) = 100)
  done
;;

let benchmark_top_tree_6 () =
  let t = Tree (Tree (Empty, 3, Empty), 2, Tree (Empty, 4, Empty)) in
  let t2 = Tree (t, 5, t) in
  let t3 = Tree (t2, 5, t2) in
  let t4 = Tree (t3, 5, t3) in
  let t5 = Tree (t4, 5, t4) in
  let t6 = Tree (t5, 5, t5) in
  let t7 = Tree (t6, 100, t6) in
  for _ = 1 to n_iter / 100 do
    assert (top (zip t7 t7) = Some (100, 100))
  done
;;

let benchmark_top_gtree_6 () =
  let t = TreeG (TreeG (EmptyG, 3, EmptyG), 2, TreeG (EmptyG, 4, EmptyG)) in
  let t2 = TreeG (t, 5, t) in
  let t3 = TreeG (t2, 5, t2) in
  let t4 = TreeG (t3, 5, t3) in
  let t5 = TreeG (t4, 5, t4) in
  let t6 = TreeG (t5, 5, t5) in
  let t7 = TreeG (t6, 100, t6) in
  for _ = 1 to n_iter / 100 do
    assert (topG (zipG t7 t7) = (100, 100))
  done
;;

let benchmark_top () =
  let open Printf in
  printf
    "\n%s%s  %-20s  %6s  %6s%s\n"
    "[1;4m"
    "Test #"
    "Description"
    "tree"
    "gtree"
    "[0m";
  let print_benchmark n desc tree gtree =
    printf "  %i     %-20s  %.3fs  %.3fs\n%!" n desc tree gtree
  in
  print_benchmark 1 "top" (time benchmark_top_tree_1 ()) (time benchmark_top_gtree_1 ());
  print_benchmark
    2
    "top incr^4"
    (time benchmark_top_tree_2 ())
    (time benchmark_top_gtree_2 ());
  print_benchmark 3 "depth" (time benchmark_top_tree_3 ()) (time benchmark_top_gtree_3 ());
  print_benchmark
    4
    "top incr^4 big"
    (time benchmark_top_tree_4 ())
    (time benchmark_top_gtree_4 ());
  print_benchmark
    5
    "top swivel^4 big"
    (time benchmark_top_tree_5 ())
    (time benchmark_top_gtree_5 ());
  print_benchmark
    6
    "top zip_tree big"
    (time benchmark_top_tree_6 ())
    (time benchmark_top_gtree_6 ())
;;

let () = benchmark_top ()
