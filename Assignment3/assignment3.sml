(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


(* Write a function only_capitals that takes a string list and returns a string list that has only the strings
 in the argument that start with an uppercase letter. Assume all strings have at least 1 character*)

val only_capitals = List.filter (fn x => Char.isUpper (String.sub (x, 0))) 

(* Write a function longest_string1 that takes a string list and returns the longest string in the list. If the 
 list is empty, return "". In the case of a tie, return the string closest to the beginning of the list.*)

fun longest_string1 lst = foldl (fn (y, acc) => if String.size y >= String.size acc then y else acc) "" lst

fun longest_string2 lst = foldl (fn (y, acc) => if String.size y > String.size acc then y else acc) "" lst


