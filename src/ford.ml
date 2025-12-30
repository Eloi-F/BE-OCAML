open Graph
open Tools

(*COMMENTAIRE DE TEST6*)

let out_arc_not_null arc_list = List.fold_left (fun acu x -> if(x.lbl = 0) then acu else x::acu) [] arc_list

let parcours_profondeur gr root dest = 

  let graph_init = new_node empty_graph root in 
  
  let rec parcours list_arc id_root  acu graph = 
    
    match list_arc with
    |  [] -> graph
    |  { src; tgt; lbl }::rest -> 
      if List.exists (fun node -> tgt = node) acu then parcours rest id_root  acu graph 
      else if tgt = dest then add_arc (new_node graph tgt) src tgt lbl
      else parcours (List.append (out_arc_not_null (out_arcs gr tgt)) rest) id_root  (tgt::acu) (add_arc (new_node graph tgt) src tgt lbl) in

  parcours (out_arc_not_null(out_arcs gr root)) root [root] graph_init


let find_arc_target gr target = 
  let get_arcs_target = e_fold gr (fun acu x -> if(x.tgt = target) then x::acu else acu ) [] in
    List.hd get_arcs_target

  
let chemin_augmentant gr src dest = 
  if node_exists gr dest then
  let graph_init = new_node empty_graph dest in

    let rec reverse_parcours_builder graph src current_node =
    
      match (node_exists graph src) with
      | true -> graph
      | false -> 
        let arc_sortant = find_arc_target gr current_node in
          reverse_parcours_builder (add_arc (new_node graph arc_sortant.src) arc_sortant.src arc_sortant.tgt arc_sortant.lbl) src arc_sortant.src
      in
      reverse_parcours_builder graph_init src dest
    else empty_graph

let chemin_to_list_arc chemin =  e_fold chemin (fun acu x -> x::acu ) []

let is_empty graph = 
  match chemin_to_list_arc graph with
  | [] -> true
  | _ -> false

let get_lowest_weight chemin = e_fold chemin (fun comp x -> if(x.lbl < comp || comp = -1) then x.lbl else comp) (-1)

let update_flow graph chemin = 
  if (is_empty chemin) then (graph, false) else
  let list_chemin = chemin_to_list_arc chemin in
  let min_value = get_lowest_weight chemin in
  let rec update_graph_with_list gr list_arc =
    match list_arc with
    | [] -> (gr, true)
    | {src; tgt; _}::rest -> update_graph_with_list (add_arc (add_arc gr src tgt (-min_value)) tgt src min_value) rest
  in update_graph_with_list graph list_chemin

let rec fordfulkerson graph src dest  = 
    match update_flow graph (chemin_augmentant (parcours_profondeur graph src dest) src dest) with
      | (gr,true)-> fordfulkerson gr src dest
      | (gr,false) -> gr
