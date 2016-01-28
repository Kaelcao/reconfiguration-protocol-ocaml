(* Declare type that will be use to model the architecture and the wire *)
type wire = {src:string;dst:string;wire_type:string}
type architecture = 
	{
		start:string list ref;
		stop:string list ref;
		fail:string list ref;
		wires:wire list ref
	}
(*
	Assume every component will be present by a string
	all component in stated state will be stored in started_components array
	all component in stopped state will be stored in stopped_components array
	all component in failed state will be stored in started_components array
*)

(* define the wire state as constant *)
let mandatory_state = "mandatory";;
let optional_state = "optional";;

(* the array of components by state *)
let started_components = ref ["d";"c";"c1";"c2"];;
let failed_components = ref [];;
let stopped_components = ref [];;

(* Create all the wires *)
let w1 =  {src="d";dst="c";wire_type=mandatory_state};;
let w2 = {src="c1";dst="c";wire_type=optional_state};;
let w3 = {src="c";dst="c1";wire_type=mandatory_state};;
let wires = ref [w1;w2;w3];;

(* Define the current architecture *)
let current_architecture = 
						{
							start=started_components;
							stop=stopped_components;
							fail=failed_components;
							wires=wires
						};;

(* The destination architecture *)
let started_components_dst = ref ["d";"c";"c1";"c2"];;
let failed_components_dst = ref [];;
let stopped_components_dst = ref [];;
let w1_dst = {src="d";dst="c";wire_type=mandatory_state};;
let w2_dst = {src="c1";dst="c";wire_type=optional_state};;
let w3_dst = {src="c";dst="c2";wire_type=mandatory_state};;
let wires_dst = ref [w1_dst;w2_dst;w3_dst];;

let destination_architecture = 
							{
								start=started_components_dst;
								stop=stopped_components_dst;
								fail=failed_components_dst;
								wires=wires_dst	
							}

(* 
---------helper functions--------- ssss
put all helper functions in here
*)

(* Remove element form a  list *)
let rec remove e l = 
	match l with
	| [] -> []
	| h::t -> if (h=e) then t else h::(remove e t);;

(*
	compare 2 wires whether they the same
*)
let compare_wire ele1 ele2 = (ele1.src = ele2.src && ele1.dst=ele2.dst && ele1.wire_type=ele2.wire_type);;

let rec remove_wire wire wire_list = 
	match wire_list with
	|[]->[]
	|h::t -> if (compare_wire h wire) then t 
			else h::remove_wire wire t;;

let rec print_list l =
	match l with
	 | [] -> ()
	 | e::l -> print_string e; print_string ";"; print_list l;;

let print_wire w = 
	print_string "{src=\"";
	print_string w.src;
	print_string "\";dst=\"";
	print_string w.dst;
	print_string "\";wire_type=\"";
	print_string w.wire_type;
	print_string "\"}";;

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
let construct component = (print_string "Construct: ";print_string component ;
						print_string "\n"; stopped_components := (!stopped_components@[component]));;

(*
	Start a component
	Remove component from the stopped state
	Then add it to the started array to move change it to started state
*)
let start component = 
		print_string "Start: ";print_string component;print_string "\n";
		(started_components := (!started_components@[component]));
	    (stopped_components := (remove component !stopped_components));;


(*
	Stop a component
	Remove component from the stated state
	Then add it to the stopped array to move change it to stopped state
*)
let stop component = 
	print_string "Stop: ";print_string component;print_string "\n";
	(stopped_components := (!stopped_components@[component]));
	(started_components := (remove component !started_components));;

(*
	Destruct a component
	Remove component from the stopped_components list
*)
let destruct component = 
	stop component;
	print_string "Destruct: ";print_string component;print_string "\n";
	(stopped_components := (remove component !stopped_components));;

(*
	Create a wire between 2 component 
	Create wire with its state
	Then add it to the wires array 
*)
let wire com_src com_dst wire_state = 
	print_string "Wire: ";print_wire {src=com_src;dst=com_dst;wire_type=wire_state};
	print_string "\n";
	(wires := (!wires@[{src=com_src;dst=com_dst;wire_type=wire_state}]));;

(*
	Remove a wire between 2 component 
	by remove it from the wires array 
*)
let rec unwire wire = 
	print_string "unwire: ";print_wire wire;print_string "\n";
	(wires := (remove_wire wire !wires));;

(* --------- End Reconfiguration Operations --------- *)



(* ---------- Begin Propagation rule helper functions ----------*)

let mandatory wire = (wire.wire_type = mandatory_state);;

(* ---------- End Propagation rule helper functions ----------*)



(* --------- Begin Propagation Rules --------- *)

let rec stopped c = 
	let rec stopped_rec c w =
		match w with
		|[]->()
		|w::t -> if (w.dst=c) then
				if (mandatory w) then stopped w.src
				else unwire w;
				stopped_rec c t
	in stopped_rec c !wires;
	stop c;;

let unwired w = 
	if (mandatory w) then 
	stopped(w.src);
	unwire w;;

let destructed c =
	let rec destructed_rec c w =
		match w with
		|[] -> ()
		| w::t -> if (w.src=c || w.dst=c) then unwired w; destructed_rec c t
	in destructed_rec c !wires;
	destruct c;;


(* --------- End Propagation Rules --------- *)

(* --------- Begin Propagation Protocol --------- *)
let wire_operation_list = ref [];;
let wire_input_list = ref [];;

(*
	return true if the wire element exist in the wires list
	else return false
*)
let rec exist_wire element wires = 
	match wires with
	|[]->false
	|h::t->if compare_wire h element then true else exist_wire element t;;
(*
	return true if the element i
*)

(*
	Get the list of Apply Down Set for unwired operation
*)
let rec diff_down_wire_list ac_wire_list ad_wire_list = 
	match ac_wire_list with
	|[] -> ()
	|h::t-> if (not (exist_wire h ad_wire_list)) then 
			(wire_operation_list := !wire_operation_list@["unwired"];
			wire_input_list := !wire_input_list@[h];
			print_string "unwired ";print_wire h)
			else diff_down_wire_list t ad_wire_list;;

(*
	Get the list of Apply Down Set for Stopped element
*)


(* --------- End Propagation Protocol --------- *)


