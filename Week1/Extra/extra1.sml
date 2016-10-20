exception EmptyList


fun is_positive x = x > 0
fun is_divisible_by(a,b) = a mod b = 0
fun divide_by(a, b) = if a<b
                      then 0
                      else 1+divide_by(a-b, b)

fun gcd(a, b) = if b=0 then a else gcd(b, a mod b)
fun lcm(a, b) = (a * b) div gcd(a, b)                                      

fun any_divisible_by(xs, n) = case xs of
                                  nil => false
                               | x::xs'=> is_divisible_by(x, n) orelse
                                    any_divisible_by(xs', n)


fun gcd_list xs = case xs of
                      nil => raise EmptyList
                    |  x::[] => x
                    | x::xs' => gcd(x, gcd_list(xs'))


fun safe_divide_by(a, b) = if b<=0
                           then NONE
                           else SOME (a div b)

fun add_opt(opt_1, opt_2) = case (opt_1, opt_2) of
                                (SOME a, SOME b) => SOME (a+b)
                              | _ => NONE

fun add_all_opt(xs) = case xs of
                          nil => NONE
                        | x::xs' => case (x, add_all_opt(xs')) of
                                       (SOME n, NONE) => SOME n
                                     | (SOME n, SOME m) => SOME (n+m)
                                     | (NONE, SOME n) => SOME n
                                     | _ => NONE
                                        
fun alternate xs = 
  let fun loop (xs, flag) =
      case xs of
          nil => 0
       | x::xs' =>  if flag
                    then x + loop(xs', not flag)
                    else ~ x + loop(xs', not flag)
  in
      loop(xs, true)
  end
                        

fun min_max xs =
  let fun min xs = foldl (fn(x, acc) => if x < acc then x else acc) (hd xs) (tl xs)
      fun max xs = foldl (fn(x, acc) => if x > acc then x else acc) (hd xs) (tl xs)       
  in
    (min xs, max xs)
  end         
        
fun unzip (xs : (int * int) list) =
  if null xs
  then ([], [])
  else |  (a, b)::xs' => case unzip(xs') of
                                                            (l1, l2) => (a::l1, b::l2)
  
      
              


                       
              
