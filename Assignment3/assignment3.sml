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

fun all_answers f lst = 
  foldl(fn (x,acc) => case acc of
			  NONE => NONE
			| SOME xs => case f x of
					 NONE => NONE
				       | SOME ys => SOME (ys @ xs)) (SOME []) lst
		      




(* Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard patterns it
 contains*)
(* 
 fun count_wildcards ptrn = 
   let fun f1 _ = 1
       fun f2 _ = 0
   in
       g f1 f2 *)
val count_wildcards = g (fn (_) => 1) (fn (_) => 0)

(* Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns the number of 
 Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable patterns 
 it contains. *)

val count_wild_and_variable_lengths = g (fn (_) => 1) (fn (x) => String.size x) 

(* Use g to define a function count_some_var that takes a string and a pattern (as a pair) and 
 returns the number of times the string appears as a variable in the pattern.*)

fun count_some_var ptrn str = g (fn (_) => 0) (fn (x) => if x=str then 1 else 0) ptrn

(* Write a function check_pat that takes a pattern and returns true if and only if all the variables appearing
 in the pattern are distinct from each other. *)

val check_pat = 
  let fun ptrn_strings ptrn = 
	  case ptrn of 
	      Variable s => [s]
	    | TupleP ps => List.foldl (fn (p,i) => (ptrn_strings p) @ i) [] ps
	    | ConstructorP (_,p) => ptrn_strings p 
	    | _ => []
      fun all_unique lst = 
	case lst of 
	    [] => true
	  | x::xs => if List.exists (fn (y) => y=x) xs
		     then false
		     else all_unique xs
  in (all_unique o ptrn_strings)
  end

(* Write a function match that takes a value * pattern and returns a (string * valu) list option, namely NONE if
 the pattern does not match and SOME lst where lst is the list of bindings if it does. *)

fun match value_pattern = 
  case value_pattern of
      (_, Wildcard) => SOME []
    | (v, Variable s)  => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const x, ConstP y) => SOME []
    | (Tuple vs,TupleP ps) => 
      let val same_length = (List.length vs) = (List.length ps)
	  val zipped_list = ListPair.zip (vs, ps)
      in
	  if same_length 
	  then all_answers match zipped_list 
	  else NONE
      end
    | (Constructor(s2, v), ConstructorP(s1, p)) => if s1=s2 
						   then match (v,p) 
						   else NONE
    | _ => NONE
  
(* Write a function first_match that takes a value and a list of patterns and returns a (string * val) list option
 namely NONE if no pattern in the list matches or SOME lst where lst is the list of bindings for the first pattern
 in the list that matches. *)

fun first_match value ptrn_list = 
  SOME (first_answer match (map (fn (x) => (value,x)) ptrn_list)) 
      handle NoAnswer => NONE


(* Write a function typecheck_patterns that "type-checks" a pattern list. *)
