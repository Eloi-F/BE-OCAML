open Gfile
open Tools

open Ford

    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)

  let graph = from_file infile in
  
  (*** test gmap ***)
  (*
  let add x = string_of_int (int_of_string x + 2) in
  let graph = gmap graph add in *)

  (*** test add_arc ***)
  
  let graph = gmap graph int_of_string in
  
  let graph = fordfulkerson graph source sink in
  
  let graph = gmap graph string_of_int in 

  (* Rewrite the graph that has been read. *)
  let () = export outfile graph in 
  ()

