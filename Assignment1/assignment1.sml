(* Write a function that takes two dates and evaluates to true or false. It evaluates to true if the
 first argument is a date that comes before the second argument. (if the two dates are the same,
it evaluates to false.*)
fun is_older (date_one : int*int*int, date_two : int*int*int) = 
  if (#1 date_one) > (#1 date_two) 
  then false
  else if (#2 date_one) > (#2 date_two)
  then false
  else (#3 date_one) < (#3 date_two)


(* Write a function number_in_month that takes a list of dates and a month and returns how many dates in the list are in the given month *)
fun number_in_month (date_list : (int*int*int) list, month : int) = 
  let fun number_in_month (date_list : (int * int * int) list) = 
	if null date_list
	then 0
	else 
	    let val cur_month = (#2 (hd date_list))
	    in 
		if cur_month = month 
		then 1 + number_in_month((tl date_list))
		else number_in_month((tl date_list))
	    end
  in number_in_month(date_list)
  end


(* Write a function number_in_months that takes a list of dates and a list of months and returns
the number of dates in the list that are in any of the months in the list of months *)
fun number_in_months (date_list : (int*int*int) list, month_list : int list) = 
  if null date_list
  then 0
  else
      let fun number_in_months (month_list : int list) = 
	    if null month_list
	    then 0
	    else
		let 
		    val cur_month = hd month_list
		    val num_times = number_in_month(date_list, cur_month)
		in num_times + number_in_months((tl month_list))
		end
      in number_in_months(month_list)
      end

(* Write a function dates_in_month that takes a list of dates and a month and returns a list holding
 the dates from the argument list of dates that are in the month. The returned list should contain 
 dates in the order they were originally given*)
fun dates_in_month (date_list : (int*int*int) list, month : int) = 
  if null date_list
  then []
  else 
      let fun dates_in_month (date_list : (int*int*int) list) = 
	    if null date_list
	    then []
	    else
		let 
		    val cur_date = hd date_list
		    val cur_month = #2 cur_date
		    val rest = dates_in_month(tl date_list)
		in 
		    if cur_month = month
		    then cur_date::rest
		    else rest
		end
      in dates_in_month(date_list)
      end


(* Write a function dates_in_months that takes a list of dates and a list of months and returns a list
 holding the dates from the argument list of dates that are in any of the months in the list of months*)
fun dates_in_months (date_list : (int*int*int) list, month_list : int list) = 
  if null date_list orelse null month_list
  then []
  else 
      let fun dates_in_months (month_list : int list) = 
	    if null month_list
	    then []
	    else
		let 
		    val cur_month = hd month_list
		    val dates = dates_in_month(date_list, cur_month)
		    val rest = dates_in_months(tl month_list)
		in dates @ rest
		end
      in dates_in_months(month_list)
      end

(* Write a function get_nth that takes a list of string and an int n and returns the nth element of the 
list where the head of the list is 1st.  *)
fun get_nth (strings : string list, elem : int) = 
  if elem=1
  then hd strings
  else  get_nth(tl strings, elem-1)
      
(* Write a function date_to_string that takes a date and returns a string of the form January 20, 2013*)
fun date_to_string (date : (int*int*int)) = 
  let
      val month_names = ["January", "February", "March","April", "May", "June","July", "August", "September", "October", "November", "December"]
      val month_int = #2 date
      val month_string = get_nth(month_names, month_int)
      val year_str = Int.toString(#1 date)
      val day_str = Int.toString(#3 date)
  in month_string ^ " " ^ day_str ^ ", " ^ year_str
  end

(* Write a function number_before_reaching_sum that takes an int called sums, which you can assume
 is positive, and an int list, which you can assume contains all positive numbers, and returns an
 int. You should return an int n such that the first n elements of the list add to less than sum, but 
 the first n + 1 elements of the list add to sum or more*)

fun number_before_reaching_sum (sum : int, num_list : int list) = 
  let fun iter (running_sum : int, cur_elem : int, cur_list : int list) = 
	let val new_sum = running_sum + (hd cur_list)
	in if new_sum >= sum
	   then cur_elem
	   else iter(new_sum, cur_elem + 1, (tl cur_list))
	end
  in iter(0, 0, num_list)
  end

(* Write a function what_month that takes a day of year and returns what month that day is in *)
fun what_month (day_of_year : int) = 
  let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in number_before_reaching_sum(day_of_year, days_in_months) + 1
  end

(* Write a function month_range that takes two days of the year day1 and day2 and returns an int list
 [m1, m2, ..., mn] where m1 is the month of day1, m2 is the month of day1+1,...,and mn is the month of day day2. *)
fun month_range (day1 : int, day2 : int) = 
  let 
      fun get_months(day : int) = 
	let val cur_month = what_month(day)
	in
	    if day=day2 
	    then cur_month::[]
	    else cur_month::get_months(day+1)
	end
  in get_months(day1)
  end

(* Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It 
 evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.*) 
fun oldest (dates : (int*int*int) list) = 
  if null dates
  then NONE
  else 
      let 
	  fun get_oldest (cur_oldest : (int*int*int), dates : (int*int*int) list) = 
	    if null dates
	    then cur_oldest
	    else if is_older(cur_oldest, hd dates)
	    then get_oldest(hd dates, tl dates)
	    else get_oldest(cur_oldest, tl dates)
      in SOME (get_oldest(hd dates, tl dates))
      end

(* Write functions number_in_month_challenge and dates_in_months_challenge that are like your
 solutions to problems 3 and 5 except having a month in the second argument multiple times has no 
 more effect than having it once*)

(* Return whether num is contained in list *)
fun contains(list : int list, num : int) = 
  if null list
  then false
  else if num=(hd list)
  then true
  else contains((tl list), num)
	       
fun number_in_months_challenge (date_list : (int*int*int) list, month_list : int list) = 
  if null date_list orelse null month_list
  then 0
  else 
      let
	  (* Same as number_in_months but keep a list of months we've already checked*)
	  fun number_in_months_challenge(month_list : int list, checked_months : int list) = 
	    if null month_list
	    then 0 
	    else if contains(checked_months, (hd month_list))
	    then number_in_months_challenge((tl month_list), checked_months)
	    else number_in_month(date_list, (hd month_list)) + number_in_months_challenge((tl month_list), (hd month_list::checked_months))
      in number_in_months_challenge(month_list, [])
      end


fun dates_in_month_challenge (date_list : (int*int*int) list, month_list : int list) = 
  if null date_list orelse null month_list
  then []
  else 
      let fun remove_duplicates (nums : int list, running_list : int list) = 
	if null nums
	then running_list
	else if contains(running_list, (hd nums))
	then remove_duplicates((tl nums), running_list)
	else remove_duplicates((tl nums), (hd nums)::running_list)
      in dates_in_months(date_list, remove_duplicates(month_list, []))
      end

fun reasonable_date (date : (int*int*int)) = 
  let val year = #1 date
      val month = #2 date
      val day = #3 date
      val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
      val days_in_month_leap_year = [31,29,31,30,31,30,31,31,30,31,30,31]
      fun is_leap_year () = 
	(year mod 400 = 0 orelse year mod 4 = 0) andalso not ((year mod 100) = 0)
      fun valid_year () = 
	year >= 1
      fun valid_month () = 
	month >= 1 andalso month < 13
      fun get_nth (num_list : int list, num : int) =
	if num=1
	then (hd num_list)
	else get_nth ((tl num_list), num-1)
      fun num_days_in_month () =
	if is_leap_year ()
	then get_nth(days_in_month_leap_year, month)
	else get_nth(days_in_month, month)
      fun valid_day () = 
	let val num_days = num_days_in_month ()
	in day <= num_days andalso day > 0
	end
  in valid_year() andalso valid_month() andalso valid_day()
  end
