open Cycle

exception BadCycle

let lhs_rhs_types_match lhs_types rhs_types = match (lhs_types, rhs_types) with
  | (first_lhs::lhs_types, rhs_types) ->
    let rec go lhs_types rhs_types = begin match (lhs_types, rhs_types) with
      | ([], [last_rhs]) -> last_rhs = first_lhs
      | (l::lhs_types, r::rhs_types) -> l == r && go lhs_types rhs_types
      | _ -> failwith "lists expected to match in length" end in
    go lhs_types rhs_types
  | _ -> raise BadCycle (* empty *)

let next_location_name = function
  | 'z' -> failwith "End of alphabet reached (go add more names)"
  | letter -> Char.chr (1 + Char.code letter)

let test_from_cycle cycle =
  let lhs_types = List.map lhs_type cycle in
  let rhs_types = List.map rhs_type cycle in
  if not (lhs_rhs_types_match lhs_types rhs_types) then raise BadCycle else
  