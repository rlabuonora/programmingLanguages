fun is_older (d1: int * int * int, d2: int * int * int) = 
  if #1 d1 = #1 d2          (* if they are in the same year *)
  then                      (* check the month *)
    if #2 d1 = #2 d2        (* if they are also in the same month *)
    then 
      if #3 d1 = #3 d2      (* check the day *)
      then  false           (* if they are also in the same day eval to false *)
      else #3 d1 < #3 d2    (* same year, same month, diff month check bigger day *)
    else #2 d1 < #2 d2      (* same year, different month, check bigger month *)
  else #1 d1 < #1 d2        (* different year check bigger year *)




fun number_in_month(dates: (int * int * int) list, month: int) = 
  if null dates
  then 0
  else
    if (#2 (hd dates)) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)


fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates: (int * int * int) list, month: int) = 
  if null dates
  then []
  else 
    if (#2 (hd dates))= month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
    
fun dates_in_months(dates: (int * int * int) list, months: int list) = 
  if null months
  then []
  else
    let 
      fun append(xs: (int * int * int) list, ys: (int * int * int) list) = 
        if null xs
        then ys
        else hd xs :: append(tl xs, ys)
    in
      append(dates_in_month(dates, hd months), dates_in_months(dates, tl months))
    end

fun get_nth(xs: string list, n: int) = 
  if n=1
  then hd xs
  else get_nth(tl xs, n-1)


fun date_to_string(date: int * int * int) = 
  let val months = ["January", "February", "March", "April",
                    "May", "June", "July", "August", "September", 
		    "October", "November", "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^", " ^ Int.toString(#1 date)
  end


fun number_before_reaching_sum(x : int, xs: int list) =  
  if (x-(hd xs))<=0
  then 0
  else 1+number_before_reaching_sum(x- hd xs, tl xs)


fun what_month(day : int) = 
  let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_months) + 1
  end

fun month_range(d1:int, d2: int) = 
  if d1>d2
  then []
  else 
    what_month(d1) :: month_range(d1+1, d2)

fun oldest(dates: (int*int*int) list) = 
  if null dates
  then NONE
  else let val tl_ans = oldest(tl dates)
    in if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
      then tl_ans
      else SOME (hd dates)
    end
