(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, lst) =
    case lst of
	[] => NONE
     |  first::rest =>
	    if same_string(s, first)
	    then SOME rest
	    else
		case all_except_option(s, rest) of
		    NONE => NONE
	          | SOME rec_result => SOME (first::rec_result)



fun get_substitutions1 (lst, s) =
    case lst of
	[] => []
      | cur_options::rest =>
	    case all_except_option(s, cur_options) of
		NONE => get_substitutions1(rest, s)
	      | SOME selected => selected @ get_substitutions1(rest, s)

							      

fun get_substitutions2 (lst, s) =
    let fun helper(lst, s, result) =
	    case lst of
		[] => result
	      | cur_options::rest =>
		    case all_except_option(s, cur_options) of
			NONE => helper(rest, s, result)
		      | SOME selected => helper(rest, s,result@selected)
    in
	helper(lst, s, [])
    end



fun similar_names (lst, {first=fir,middle=mid,last=las}) =
    (*Using local helper func bc result from get_substituions2() 
      shouldn't be included in recursive calls*)
    let fun helper (substitutions) =
	    case substitutions of
		[] => [{first=fir,middle=mid,last=las}]
	      | substitution::rest => {first=substitution,middle=mid,last=las}::helper(rest)
    in
	helper(get_substitutions2(lst, fir)) 
    end	


(*
fun tail_rec_similar_names (lst, {first=fir,middle=mid,last=las}) =
    let fun helper (substitutions, result) =
	    case substitutions of
		[] => result
	      | substitution::rest => helper(rest, {first=substitution,middle=mid,last=las}::result)
    in
	helper(get_substitutions2(lst, fir), [{first=fir,middle=mid,last=las}])
    end	
*)

	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (suit, rank) =
    case suit of
        Clubs => Black
      | Spades => Black
      | _ => Red
		 
		    
		      
fun card_value (suit, rank) =
    case rank of
	Num value => value
      | Ace => 11
      | _ => 10

		 

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | first_card::rest =>
	    if first_card = c
	    then rest
	    else first_card::remove_card(rest, c, e)



fun all_same_color cs =
    case cs of
	[] => true
      | _::[] => true
      | fir_card::sec_card::rest => card_color(fir_card)=card_color(sec_card)
				    andalso all_same_color(sec_card::rest)
								   
	    
	
fun sum_cards cs =
    let fun helper (cs, acc) =
	    case cs of
		[] => acc
	      | first_card::rest => card_value(first_card) + sum_cards(rest)
    in
	helper(cs, 0)
    end



fun score (held_cards, goal) =
    let val sum = sum_cards (held_cards);
	val prel_score = if sum > goal
			 then 3 * (sum - goal)
			 else goal - sum
    in		    
	if all_same_color (held_cards)
	then prel_score div 2
	else prel_score
    end					 



fun officiate (card_list, move_list, goal) =
    let fun run (card_list, held_cards, moves) =
            case moves of
		[] => score(held_cards, goal)
	      | Discard(c)::rest_moves => run(card_list, remove_card(held_cards,c,IllegalMove), rest_moves)
	      | Draw::rest_moves =>
		    case card_list of
			[] => score(held_cards, goal)
		      | drew::rest_cards => run(rest_cards, drew::held_cards, rest_moves)
		
    in
	run(card_list, [], move_list)
    end


fun officiate (cards,plays,goal) =
    let 
        fun loop (current_cards,cards_left,plays_left) =
            case plays_left of
                [] => score(current_cards,goal)
              | (Discard c)::tail => 
                loop (remove_card(current_cards,c,IllegalMove),cards_left,tail)
              | Draw::tail =>
                (* note: must score immediately if go over goal! *)
                case cards_left of
                    [] => score(current_cards,goal)
                  | c::rest => if sum_cards (c::current_cards) > goal
                               then score(c::current_cards,goal)
                               else loop (c::current_cards,rest,tail)
    in 
        loop ([],cards,plays)
    end
