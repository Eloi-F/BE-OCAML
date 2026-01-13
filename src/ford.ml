open Graph
open Tools
open Gfile

(* -----------------------------------------------------------
  parcours_profondeur : gr root dest -> graph

  Effectue un parcours en profondeur sur gr en partant de root
  jusqu'à trouver dest. Renvoie le graphe complet de 
  l'exploration de gr contenant le parcours.
*)

let out_arc_not_null arc_list = List.fold_left (fun acu x -> if(x.lbl = 0) then acu else x::acu) [] arc_list

let parcours_profondeur gr root dest = 
  (*On initialise un graphe avec la racine*)
  let graph_init = new_node empty_graph root in 
  
  let rec parcours list_arc id_root  acu graph = 
    
    match list_arc with
    |  [] -> graph (*Plus d'arc à visiter sur ce noeud*)
    |  { src; tgt; lbl }::rest -> 

      (*Arc déjà visité => on passe au suivant*)
      if List.exists (fun node -> tgt = node) acu then parcours rest id_root  acu graph 

      (*Puits atteint => on a trouvé un chemin de src à dest*)
      else if tgt = dest then add_arc (new_node graph tgt) src tgt lbl 

      (*Appel récursif, on ajoute les nouveaux arcs à tester, on marque le noeud courant comme déjà visité, on update le graphe*)
      else parcours (List.append (out_arc_not_null (out_arcs gr tgt)) rest) 
                    id_root  
                    (tgt::acu) 
                    (add_arc (new_node graph tgt) src tgt lbl) in

  (*Parcours va remplir le graphe en partant de la racine*)
  parcours (out_arc_not_null(out_arcs gr root)) root [root] graph_init



(* -----------------------------------------------------------
  chemin_augmentant gr src dest -> graph

  Trouve le chemin augmentant de src vers dest à partir d'un 
  parcours en profondeur donné en entrée. On trouve ce chemin 
  en parcourant gr de la destination vers la source pour 
  garantir l'unicité du résultat trouvé. Renvoie le graphe
  associé à ce parcours. 
*)
let find_arc_target gr target = 
  let get_arcs_target = e_fold gr (fun acu x -> if(x.tgt = target) then x::acu else acu ) [] in
    List.hd get_arcs_target

let chemin_augmentant gr src dest = 
  (*Si il est possible d'atteindre le puits*)
  if node_exists gr dest then
  let graph_init = new_node empty_graph dest in

    let rec reverse_parcours_builder graph src current_node =
    
      match (node_exists graph src) with
      | true -> graph (*On a atteint la source, processus terminé*)
      | false -> 
        (*On utilise find_arc_target pour trouver le noeud père, on l'ajoute au graphe avec l'arc qui les relie, puis appel récursif*)
        let arc_sortant = find_arc_target gr current_node in
          reverse_parcours_builder (add_arc (new_node graph arc_sortant.src) arc_sortant.src arc_sortant.tgt arc_sortant.lbl) 
                                    src 
                                    arc_sortant.src
      in
      reverse_parcours_builder graph_init src dest
  (*Sinon : Fin de l'algorithme Ford Fulkerson*)
  else empty_graph



(* -----------------------------------------------------------
  is_empty graph

  Vérifie à chaque nouvelle itération si le chemin augmentant
  obtenu est vide, auquel cas Fin de l'algorithme Ford 
  Fulkerson. 
  graph empty     -> true
  graph not empty -> false
*)
let chemin_to_list_arc chemin =  e_fold chemin (fun acu x -> x::acu ) []
let is_empty graph = 
  match chemin_to_list_arc graph with
  | [] -> true
  | _ -> false



(* -----------------------------------------------------------
  get_lowest_weight chemin

  Calcule et renvoie le poids le plus faible du chemin 
  augmentant donné en paramètre.
*)
let get_lowest_weight chemin = e_fold chemin (fun comp x -> if(x.lbl < comp || comp = -1) then x.lbl else comp) (-1)



(* -----------------------------------------------------------
  update_flow graph chemin

  Actualise les valeurs du graphe de flot donné en entrée à 
  partir du chemin augmentant donné. 
*)
let update_flow graph chemin = 
  (*Si chemin vide : Fin de l'algorithme Ford Fulkerson*)
  if (is_empty chemin) then (graph, false) else
  let list_chemin = chemin_to_list_arc chemin in
  let min_value = get_lowest_weight chemin in
  let rec update_graph_with_list gr list_arc =
    match list_arc with
    | [] -> (gr, true)
    | {src; tgt; _}::rest -> update_graph_with_list (add_arc (add_arc gr src tgt (-min_value)) tgt src min_value) rest
  in update_graph_with_list graph list_chemin



(* -----------------------------------------------------------
  fordfulkerson graph src dest

  Execute l'algorithme complet de Ford Fulkerson sur le graph
  donné en entrée, partant de la source src jusqu'au puits 
  dest. Renvoie le graphe de flot associé.
*)
let fordfulkerson graph src dest  = 
  let rec fordfulkerson_iter graph src dest i = 
    match update_flow graph (chemin_augmentant (parcours_profondeur graph src dest) src dest) with
      | (gr,true)-> export ("outfile"^(string_of_int i)^".gv.txt") (gmap gr string_of_int); fordfulkerson_iter gr src dest (i+1)
      | (gr,false) -> export ("outfile"^(string_of_int i)^".gv.txt") (gmap gr string_of_int) ; gr
  in fordfulkerson_iter graph src dest 0
