open Graph
open Tools

let parcours_profondeur gr root = 
  let graph_with_no_arc = clone_nodes gr in

  let rec parcours list_arc id acu graph = match list_arc with
    |  [] -> graph
    |  { src; tgt; lbl }::rest -> 
      if List.exists (fun node -> tgt = node) acu then parcours rest id acu graph 
      else parcours (List.append (out_arcs gr tgt) rest) id (tgt::acu) (add_arc graph src tgt lbl) in

  parcours (out_arcs gr root) root [] graph_with_no_arc
