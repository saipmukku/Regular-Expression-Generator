open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  fold_left (fun x y -> List.append x (fold_left (fun acc (a, b, c) -> 
      if a = y && b = s then (List.append (List.append [] [c]) acc) else acc) [] nfa.delta)) [] qs

let rec e_helper nfa lst final = 
  match lst with 
  | [] -> final

  | h :: t -> if List.mem h final then e_helper nfa t final
    else e_helper nfa (List.append lst (move nfa (List.append [] [h]) None)) (h :: final)

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = e_helper nfa qs []

let rec accept_helper nfa start trans = 
  match trans with
  | [] -> if intersection start nfa.fs = [] then false else true

  | h :: t -> let states = move nfa start (Some h) in 
    accept_helper nfa (e_closure nfa states) t
            
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  accept_helper nfa (e_closure nfa [nfa.q0]) (explode s) 

(*
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let final = [] in List.concat_map (function (a, b, c) -> Sets.union [c] final) 
  (List.filter (fun (a, b, c) -> if (not (List.mem a qs) && s = b) then false else true) nfa.delta)  

let rec e_closure_helper nfa lst final = match lst with 
  | [] -> final
  | h :: t -> if (List.mem h final) = 
    true then e_closure_helper nfa t final else e_closure_helper nfa 
    (Sets.union lst (move nfa (Sets.union [] [h]) None)) (h :: final)
  
let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = e_closure_helper nfa qs []

let rec accept_helper (nfa: ('q,'s) nfa_t) start trans = match trans with
  | [] -> if Sets.intersection start nfa.fs != []
    then true else false
  | h :: t -> let states = move nfa start (Some h) in
    accept_helper nfa (e_closure nfa states) t

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = accept_helper nfa (e_closure nfa [nfa.q0]) (explode s)
*)

(*
let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = let final = List.append [] qs in  
List.concat_map (function (a, b, c) -> List.append [c] final) (List.filter (function (a, b, c) -> if Sets.elem a final && b = None then true else false) nfa.delta)

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = let curr_state = q0 in 
if (List.exists (function (a, b, c) -> if a = q0 && b = (List.hd (explode s)) then true else false) nfa.delta)
&& (List.fold_right () List.tl (explode s))

let rec accept_helper (x: 'q) (trans: ('q, 's) transition) : 'q = match trans with | (a, b, c) -> c;;
*)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left(fun y x -> e_closure nfa (move nfa qs (Some x)) :: y) [] nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list = 
let folder = List.fold_left2 (fun acc trans e -> (qs, Some trans, e) :: acc) 
[] (List.rev nfa.sigma) (new_states nfa qs) in folder

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let finals = List.fold_right(fun x y -> List.mem x nfa.fs || y) qs false in
  if finals then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) (work: 'q list list) : ('q list, 's) nfa_t =
  match work with 
  | [] -> dfa
  | h :: t -> 
    if h = [] then nfa_to_dfa_step nfa dfa t else if elem h dfa.qs 
    then nfa_to_dfa_step nfa dfa t else let new_st = new_states nfa h in
    nfa_to_dfa_step nfa {sigma = dfa.sigma; qs = h :: dfa.qs; q0 = dfa.q0; 
    fs = (Sets.union (new_finals nfa h) dfa.fs);
    delta = (Sets.union (new_trans nfa h) dfa.delta)} (Sets.union new_st t)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let x = e_closure nfa [nfa.q0] in
  let dfa = {sigma = nfa.sigma; qs = []; q0 = x; fs = []; delta = []} in
  nfa_to_dfa_step nfa dfa [x]