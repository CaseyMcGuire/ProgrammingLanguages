(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Write a function all_except_option, which takes a string and a string list. Return NONE if the string
 is not in the list, else return SOME lst where lst is identical to the argument list except the string is not
 in it. You may assume the string is in the list at most once. *)

fun all_except_option (s : string, str_list : string list) = 
  let fun filter_str (lst : string list) = 
	case lst of 
	    [] => []
	  | x::xs' => if same_string(x, s) 
		      then filter_str(xs')
		      else x::filter_str(xs')
      val filtered_lst = filter_str(str_list)
  in if (length str_list) = (length filtered_lst)
     then NONE
     else SOME filtered_lst
  end

(*Write a function get_subsitutions1, which takes a string list list (a list of list of strings and string s and 
 returns a string list. The result has all the strings that are in some list in substitutions that also has s, but
 s itself should not be in the result*)
fun get_substitutions1 (str_list : string list list, s : string) = 
  let fun subs (str_list : string list list) = 
	case str_list of 
	    [] => []
	  | x::xs' => let val filtered_list = all_except_option(s, x) 
		      in case filtered_list of 
			     NONE => subs(xs')
			  |  SOME lst => lst @ subs(xs')
		      end
  in subs(str_list)
  end
	
(* Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local
 helper function*)

fun get_substitutions2 (str_list : string list list, s : string) = 
  let fun subs (str_list : string list list, acc : string list) = 
	case str_list of 
	    [] => acc
	  | x::xs' => let val filtered_list = all_except_option(s, x)
		      in case filtered_list of 
			     NONE => subs(xs', acc)
			   | SOME lst => subs(xs', lst @ acc)
		      end
  in subs(str_list, [])
  end

(*Write a function similar_names, which takes a string list list of substitutions and a full name of type {first:string, middle:string, last:string} and a returns a list of full names. The result is all the full names you can
 produce by substituting for the first name (and only the first name) using substitutions and parts b and c.*)
fun similar_names (substitutions : string list list, {first=f, middle=m, last=l}) =
  let fun replace_first_names (names : string list) = 
	let fun iter (names : string list, acc : {first_name : string, middle_name : string , last_name : string} list) = 
	    case names of 
		[] => acc
	      | x::xs' => iter(xs', {first_name=x, middle_name=m, last_name=l}::acc)
	in iter(names, [])
	end
      fun get_similar_names (subs : string list list, acc : {first_name : string, middle_name : string, last_name : string} list)=
	case subs of 
	    [] => acc
	  | x::xs' => let val filtered_list = all_except_option(f, x)
		      in case filtered_list of 
			     NONE => get_similar_names(xs', acc)
			   | SOME lst => get_similar_names(xs', replace_first_names(lst) @ acc)
		      end
  in get_similar_names(substitutions, [])
  end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Write a function card_color, which takes a card and returns its color *)
fun card_color (suit, _) = 
  case suit of 
      Diamonds => Red
    | Hearts => Red
    | _ => Black

(* Write a function card_value, which takes a card and return its value (numbered cards have their number
 as the value, aces are 11, everything else is 10*)
fun card_value (_, rank) = 
  case rank of 
      Ace => 11
   |  King => 10
   | Queen => 10
   | Jack => 10
   | Num x => x


(* Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a list
 that has all the elements of cs except c. If c is in the list more than once, remove only the first one. If c 
 is not in the list, raise the exception e. You can compare cards with *)

fun remove_card (card_list : card list, c : card, excp : exn) = 
  let fun remove_card (card_list : card list, acc : card list) = 
	case card_list of 
	    [] => raise excp
	  | x::xs' => if c=x
		      then acc @ xs'
		      else remove_card(xs', x::acc)
  in remove_card(card_list, [])
  end

(* Write a function all_same_color, which takes a list of cards and returns true if all the cards in the list are
 the same color.*)
fun all_same_color (card_list : card list) = 
  case card_list of 
      [] => true
    | x::[] => true
    | x::y::xs => card_color(x)=card_color(y) andalso all_same_color(y::xs)
  
val test = all_same_color([(Clubs, Num 10), (Clubs, Num 9)]) 


val test2 = all_same_color([(Clubs, Num 10), (Hearts, Num 9)])

(*Write a function sum_cards which takes a list of cards and returns the sum of their values. *)

fun sum_cards (card_list : card list) = 
  let fun sum_cards (card_list : card list, running_sum : int) = 
	case card_list of 
	    [] => running_sum
	  | x::xs => sum_cards(xs, running_sum + card_value(x))
  in sum_cards(card_list, 0)
  end


(* Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes the score *)
fun score (card_list : card list, goal : int) = 
  let val sum = sum_cards(card_list)
      val preliminary_score = if sum > goal 
			      then 3 * (sum - goal)
			      else (goal - sum)
      val score = if all_same_color(card_list)
		  then preliminary_score div 2
		  else preliminary_score
  in score
  end

(* Write a function officiate, which "runs a game." It takes a card list and a move list (what the player does at 
 each point), and an int (the goal) and returns the score at the end of the game after processing (some or all of) 
 the moves in the move list in order.*)

fun officiate (card_list : card list, move_list : move list, goal : int) = 
  let fun end_game (end_game_cards : card list) = 
	score (end_game_cards, goal)
      fun officiate (card_list : card list, move_list : move list, held_cards : card list) = 
	case move_list of 
	    [] => end_game(held_cards)
	  | x::xs => case x of 
			 Discard card => officiate(card_list, xs, remove_card(held_cards, card, IllegalMove))
		       | Draw => case card_list of 
				     [] => end_game(held_cards)
				   | y::ys => if sum_cards(y::held_cards) > goal
					      then end_game(y::held_cards)
					      else officiate(ys, xs, y::held_cards)
  in officiate(card_list, move_list, [])
  end
							    
(* Write a score_challenge and an officiate_challenge to be like their nonchallenging counterparts except each 
 ace can have a value of 1 or 11 and score_challenge should always return the least possible score*)

(*This is wrong... *) 
(*
fun score_challenge (card_list : card list, goal : int) = 
  let fun count_aces (card_list : card list, num_aces : int) = 
	case card_list of
	    [] => num_aces
	  | (_,Ace)::rest => count_aces(rest, num_aces + 1)
	  | (_,_)::rest => count_aces(rest, num_aces)
  in score(card_list, goal) - (10 * count_aces(card_list, 0))
  end
*)
(*
fun careful_player (card_list : card list, goal : int) = 
  let fun careful_player (card_list : card list, held_cards : card list, move_list : move list, running_score : int) = 
*)


	    
	 
		 
	    
