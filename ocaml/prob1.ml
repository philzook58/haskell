open List
(*last : 'a list -> 'a option *)
let rec last l = match l with
	| [] -> None
	| a :: [] -> Some a
	| a :: bs -> last bs;;

let rec last_two l = match l with
	| [] -> None
	| a :: [] -> None
	| b :: a :: [] -> Some b
	| a :: bs -> last_two bs;;

let rec at n l = match n with
	| 0 -> Some (hd l) 
	| n -> at (n-1) (tl l);;

let is_palindrome l = 
	let revl = rev l in
	let rec compare l1 l2 = match l2 with
	| [] -> true
	| a::bs -> if ((hd l1) = a) then (compare (tl l1) bs) else false in
	compare revl l;;


