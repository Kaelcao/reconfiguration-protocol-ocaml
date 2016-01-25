type wire = {src:string;dst:string}

(*
	Assume every component will be present by a string
	all component in stated state will be stored in started array
	all component in stopped state will be stored in stopped array
	all component in failed state will be stored in failed array
*)
let started = ref [];;
let failed = ref [];;
let stopped = ref [];;

(* helper function *)

(* Remove element form a  list *)
let rec remove e l = 
	match l with
	| [] -> []
	| h::t -> if h=e then t else h::remove e t;;

(* 
	Construct a component 
	Add component to stopped list
*)
let construct component = (stopped := (!stopped@component));;

(*
	Destruct a component
	Remove component from the stopped list
*)
let destruct component = 
	let rec des_rec component l =
	match l with
	|[]->[]
	|x::l -> if (component=x) then stopped := l;
			 else stopped := x :: destruct 





