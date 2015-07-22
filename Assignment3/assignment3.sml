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


fun longest_string_helper f lst = 
  foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) "" lst
      


val longest_string3 = longest_string_helper (fn(x,y) => x >= y)

val longest_string4 = longest_string_helper (fn(x,y) => x > y)

(* Write a function longest_capitalized that takes a string list and returns the longest string in the list that 
 begins with an uppercase letter, or "" if there are no such strings. Assume all strings have at least one 
 character. *)

val longest_capitalized = foldl (fn (y, acc) => if (Char.isUpper o hd o String.explode) y andalso String.size y >= String.size acc then y else acc) ""
													       
(* Write a function rev_string that takes a string function and returns the string that is the same characters in
 reverse order.*)

val rev_string = (String.implode o List.rev o String.explode)

(* Write a function first_answer of type ('a -> 'b option) -> 'a list -> 'b. The first argument should be applied
 to elements of the second argument in order until the first time it returns SOME v for some v and the nb is the 
result of the call to first_answer. If the first argument returns NONE for all list elements, then first_answer 
 should raise the exception NoAnswer. *)

fun first_answer f lst = 
  case lst of 
      [] => raise NoAnswer
    | x::xs => case f x of 
		   NONE => first_answer f xs
		 | SOME y => y


(* Write a function all_answers of type ('a -> 'b list option) -> 'a list -> 'b list option. The first argument
 should be applied to elements of the second argument. If it returns NONE for any element, then the result for
 all_answers is NONE. Else the calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn
 and the result of all_answers is SOME lst where lst is lst1, lst2, .., lstn appended together (order doesn't 
 matter). *)

  
