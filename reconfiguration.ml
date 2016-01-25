
type wire = {src:string;dst:string;wire_type:string}

(*
	Assume every component will be present by a string
	all component in stated state will be stored in started_components array
	all component in stopped state will be stored in stopped_components array
	all component in failed state will be stored in started_components array
*)
let started_components = ref [];;
let failed_components = ref [];;
let stopped_components = ref [];;
let wires = ref [];;

(* 
---------helper functions--------- 
put all helper functions in here
*)

(* Remove element form a  list *)
let rec remove e l = 
	match l with
	| [] -> []
	| h::t -> if (h=e) then t else h::(remove e t);;

let rec print_list l =
	match l with
	 | [] -> ()
	 | e::l -> print_string e; print_string " "; print_list l;;

(* ---------end helper function--------- *)

(*
	get state of all component
*)
let state = 
	print_string "Started: "; print_list !started_components;
	print_string "Started: "; print_list !started_components;
	print_string "Started: "; print_list !started_components;;


(* --------- Reconfiguration Operations --------- *)
(* 
	Construct a component 
	Add component to stopped_components list
*)
let construct component = (stopped_components := (!stopped_components@[component]));;



(*
	Destruct a component
	Remove component from the stopped_components list
*)
let destruct component = (stopped_components := (remove component !stopped_components));;

(*
	Start a component
	Remove component from the stopped state
	Then add it to the started array to move change it to started state
*)
let start component = 
		(started_components := (!started_components@[component]));
	    (stopped_components := (remove component !stopped_components));;


(*
	Stop a component
	Remove component from the stated state
	Then add it to the stopped array to move change it to stopped state
*)
let stop component = component;;

(*
	Create a wire between 2 component 
	Create wire with its state
	Then add it to the wires array 
*)
let wire com_src com_dst wire_state = com_src;;

(*
	Create a wire between 2 component 
	Create wire with its state
	Then add it to the wires array 
*)
let unwire wire = wire;;


(* --------- End Reconfiguration Operations --------- *)

