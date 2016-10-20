(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* TODO: terminar esta implementacion sin flag *)
(* fun all_except_option(x, xs) = *)
(*   case xs of  *)
(*       [] => NONE *)
(*  | y::xs' => if same_string(x, y) *)
(* 	     then  *)
(*              else SOME y :: all_except_option(x, xs') *)

(* put your solutions for problem 1 here *)
fun all_except_option(x, xs) =
  let fun all_except_list(xs: string list, x: string, flag: bool, acc: string list) =
    case xs of
       []        => if flag then acc else xs
    |  x'::xs'   => if same_string(x, x')
		       then all_except_list(xs', x, true, acc)
		       else all_except_list(xs', x, flag, acc @ [x'])
  in case all_except_list(xs, x, false, []) of
     [] => NONE
  | lst => SOME lst
  end

fun get_substitutions1(xs, x) = 
  case xs of
     []       => []
  | x'::xs'   => case all_except_option(x, x') of
		  NONE     => get_substitutions1(xs', x)
		| SOME lst => lst @ get_substitutions1(xs', x)

fun get_substitutions2(xs: string list list, x: string) = 
  let fun loop(xs: string list list, acc: string list) = 
         case xs of
                 [] => acc
          | x'::xs' => case all_except_option(x, x') of
                                NONE => loop(xs', acc)
			   | SOME ys => loop(xs', acc @ ys)
  in loop(xs, [])
  end

fun similar_names(xs, {first= f, middle= m, last= l})=
  let fun helper(mid, last, subs) =
      case subs of
	 []    => []
     |  x::xs' => {first=x, middle=mid, last=last} :: helper(mid, last, xs')
  in
  case xs of 
      []   => [{first=f, middle=m, last=l}]
   | y::ys => ( case all_except_option(f, y) of 
		  SOME lst => similar_names(ys, {first=f, middle=m, last=l}) @ helper(m, l, lst) 
		 | NONE => similar_names(ys, {first=f, middle=m, last=l}))


  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


fun card_color(c) = 
  case c of
      (Clubs, _) => Black 
   |  (Spades, _) => Black 
   |  (Hearts, _) => Red
   |  (Diamonds,_) => Red


fun card_value(c) = 
  case c of 
      (_, Ace) => 11
    | (_, Num i) => i
    | _ =>  10

fun remove_card(cs, c, e) =
  let fun loop(cs, c, e, acc) = 
    case cs of
	[] => raise e
     | 	c'::cs' => if c=c' then acc @ cs' else loop(cs',c, e, acc @ [c'])
  in loop(cs, c, e, [])
  end

fun all_same_color(cs) = 
  case cs of
      [] => true
   | c::[] => true
   | c'::c''::cs' => card_color(c')=card_color(c'') andalso all_same_color(c''::cs')

fun sum_cards(cs) = 
  let fun loop(cs, acc) = 
    case cs of
	[] => acc
     | c::cs' =>  loop(cs', card_value(c) + acc)
  in loop(cs, 0)
  end


fun score(held, goal) = 
  let 
    val sum = sum_cards(held)
    val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
  in
    if all_same_color(held) 
    then preliminary_score div 2 
    else preliminary_score
  end

fun officiate(deck, moves, goal) = 
  let fun loop(deck, held_cards, moves) = 
    case moves of
             [] => score(held_cards, goal) (* no more moves *)
    | m::moves' => case m of
			   Draw => (case deck of
	                                 [] => score(held_cards, goal)
			           | c::cs' => if sum_cards(c::held_cards) > goal
                                               then score(c::held_cards, goal)
                                               else  loop(cs', c::held_cards, moves'))
                   | Discard(c) => loop(deck, remove_card(held_cards, c, IllegalMove), moves')
  in 
      loop(deck, [], moves)
  end
  
