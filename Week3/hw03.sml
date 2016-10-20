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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals xs = 
  List.filter (fn string => Char.isUpper((String.sub (string, 0)))) xs

(* 2 *)
fun longest_string1 xs = 
  foldl (fn (acc, x) => if (String.size x >= String.size acc)
                         then x
                         else acc)
	 "" xs

(* 3 *)
fun longest_string2 xs = 
  foldl (fn (acc, x) => if (String.size x > String.size acc)
                         then x
                         else acc)
	 "" xs
(* 4 *)
fun longest_string_helper comp xs = foldl (fn(acc, x) => 
                   				 if (comp(String.size x, String.size acc))
                                                 then x
                                                 else acc) "" xs

fun longest_string3 xs = longest_string_helper (fn(i, j) => i >= j) xs
fun longest_string4 xs = longest_string_helper (fn(i, j) => i > j) xs

(* 5 *)
fun longest_capitalized xs = (longest_string1 o only_capitals) xs

(* 6 *)
fun rev_string str = (String.implode o List.rev o String.explode) str

(* 7 *)
fun first_answer f xs = 
  case xs of
         [] => raise NoAnswer
   | x::xs' => (case f x of
		    SOME v => v
		 |  NONE   => first_answer f xs')

(* 8 *)
fun all_answers f xs = 
  let fun helper acc xs = 
    case xs of 
	[]   => SOME acc
    | y::ys  => case f y of
                     NONE => NONE
	       | SOME ys' => helper (ys' @ acc) ys
  in helper [] xs

  end
      
(* 9a *)
fun count_wildcards p = g (fn () => 1) (fn x => 0) p;

(* 9b *)
fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p;

(* 10 *)
fun count_some_var (str, p) = g (fn () => 0) (fn x => if x=str then 1 else 0) p;

(* 11 *)
fun check_pat p =
  let
    fun variables p =
      case p of
	Variable x        => [x]
      | TupleP ps         => List.foldl (fn (p,i) => (variables p) @ i) [] ps
      | ConstructorP(_,p) => variables p
      | _                 => []
    fun has_duplicates xs = 
      case xs of
	nil => false
       | x::xs' => List.exists (fn y => x=y) xs' orelse (has_duplicates xs')
    in
      not ((has_duplicates o variables) p)
    end

(* 12 *)
fun match (valu, pattern) =
  case (valu, pattern) of
    (_, Wildcard) => SOME [] 
   | (v, Variable s) => SOME [(s, v)]
   | (Unit, UnitP) => SOME []
   | (Const i, ConstP j) => if i=j then SOME [] else NONE
   | (Tuple vs, TupleP ps) => if length vs = length ps 
			      then all_answers match (ListPair.zip(vs, ps)) 
			      else NONE
   | (Constructor (c_name1, v), ConstructorP (c_name2, p)) => 
                      if c_name1=c_name2 
                      then 
                        let val m = match(v, p) 
		        in 
			  if isSome m
                          then m
                          else NONE
                        end
                      else NONE
   | (_, _) => NONE

(* 12 *)
fun first_match valu ps =
  let
    fun matches_val (pat) = match(valu, pat);
  in
      SOME (first_answer matches_val ps)
  end
  handle NoAnswer => NONE



