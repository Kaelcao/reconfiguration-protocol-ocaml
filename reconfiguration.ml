
type wire = {src:string;dst:string;wire_type:string}

(*
	Assume every component will be present by a string
	all component in stated state will be stored in started_components array
	all component in stopped state will be stored in stopped_components array
	all component in failed state will be stored in started_components array
*)
let started_components = ref ["a"];;
let failed_components = ref ["b"];;
let stopped_components = ref ["c"];;
let wires = ref [{src="a";dst="b";wire_type="c"}];;

let mandatory_state = "mandatory";;
let optional_state = "optional";;

(* 
---------helper functions--------- ssss
put all helper functions in here
*)

(* Remove element form a  list *)
let rec remove e l = 
	match l with
	| [] -> []
	| h::t -> if (h=e) then t else h::(remove e t);;

let rec remove_wire wire wire_list = 
	match wire_list with
	|[]->[]
	|h::t -> if (h.src = wire.src && h.dst=wire.dst && h.wire_type=wire.wire_type) then t 
			else h::remove_wire wire t;;

let rec print_list l =
	match l with
	 | [] -> ()
	 | e::l -> print_string e; print_string ";"; print_list l;;

let print_wire w = 
	print_string "{src: ";
	print_string w.src;
	print_string " dst: ";
	print_string w.dst;
	print_string " type: ";
	print_string w.wire_type;
	print_string "}";;

let rec print_wire_list l =
	match l with
	| [] -> ()
	| e::l -> print_wire e;print_string "\n";print_wire_list l;;
	

(* ---------end helper function--------- *)

(*
	get state of all component
	print out all tehe value of 3 lists: started, stopped, failed
*)
let show_started = print_list !started_components;;
let show_stopped = print_list !stopped_components;;
let show_failed = print_list !failed_components;;
let show_wire = print_wire_list !wires;;
	

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
let stop component = 
	(stopped_components := (!stopped_components@[component]));
	(started_components := (remove component !started_components));;
(*
	Create a wire between 2 component 
	Create wire with its state
	Then add it to the wires array 
*)
let wire com_src com_dst wire_state = (wires := (!wires@[{src=com_src;dst=com_dst;wire_type=wire_state}]));;

(*
	Remove a wire between 2 component 
	by remove it from the wires array 
*)
let rec unwire wire = (wires := (remove_wire wire !wires));;


(* --------- End Reconfiguration Operations --------- *)

