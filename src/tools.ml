(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let  clone_nodes gr =  n_fold gr new_node empty_graph ;;

let  gmap gr f = e_fold gr (fun acu arc -> new_arc acu {src=arc.src; tgt=arc.tgt; lbl=(f arc.lbl)} ) (clone_nodes gr);;


let add_arc g id1 id2 n = match (find_arc g id1 id2) with
  | None -> new_arc g {src=id1; tgt=id2; lbl=n}
  | Some arc -> new_arc g {src=id1; tgt=id2; lbl=n+arc.lbl};;