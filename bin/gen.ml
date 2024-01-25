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

(*let next_location_name = function
  | 'z' -> failwith "End of alphabet reached (go add more names)"
  | letter -> Char.chr (1 + Char.code letter)*)

let next_location (pa_id, alias_index) relation = match location_info relation with
  | `Same -> (pa_id, alias_index)
  | `Diff -> (pa_id + 1, 0)
  | `Aliased -> (pa_id, alias_index + 1)

let rotate n xs =
  let rec go n = function
    | [] -> raise (Invalid_argument "Rotated too far")
    | (x::xs) ->
      if n = 0 then
        ((x::xs), [])
      else
        let (l, r) = go (n - 1) xs in (l, x::r) in
  let (l, r) = go n xs in
  l @ r

let apply_first_rotation cycle = (* start on a new location *)
  let rec find_rotation_point index state = function
    | [] -> state
    | [rel] -> if location_info rel = `Same then state else None
    | rel::cycle -> match location_info rel with
      | `Same -> find_rotation_point (index+1) state cycle
      | `Aliased -> find_rotation_point (index+1) (Some (Option.value state ~default:index)) cycle
      | `Diff -> Some index in
  let rotation_point = Option.value ~default:0 (find_rotation_point 1 None cycle) in
  rotate rotation_point cycle

let apply_second_rotation cycle = (* start on a new processor *)
  let rec find_rotation_point index = function
    | [] -> 0
    | (rel, _)::_ when processor_info rel = External -> index
    | _::cycle -> find_rotation_point (index + 1) cycle in
  let rotation_point = find_rotation_point 1 cycle in
  rotate rotation_point cycle

let locations_from_cycle =
  let rec go initial = function
    | [] -> []
    | (rel::cycle) -> initial :: go (next_location initial rel) cycle in
  go (0, 0)

let assign_write_values cycle =
  let rec go cycle ws = match cycle with
    | [] -> []
    | rel::cycle -> match rel with
      | ProgramOrder (loc_flag, Write, _) -> ws :: go cycle (if loc_flag = `Diff then 1 else ws + 1)
      | ReadsFrom _ -> ws :: go cycle (ws + 1)
      | FromRead _ | ProgramOrder (_, Read, _) -> go cycle ws in
  go cycle 1

let rec zip xs ys = match (xs, ys) with
  | (x::xs, y::ys) -> (x, y) :: zip xs ys
  | ([], []) -> []
  | _ -> raise (Invalid_argument "zip: Uneven lengths")

let new_reg setup value =
  let module IntMap = Map.Make(Int) in
  let new_key = IntMap.cardinal setup in
  (IntMap.add new_key value setup, new_key)

let rec setup_page_tables test = function
  | [] -> test
  | (pa, va)::locations ->
    let open Test in
    let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
    let virtual_addresses = StringSet.add va_name test.virtual_addresses in
    let pa_name = Printf.sprintf "pa_%d" pa in
    let physical_addresses = StringSet.add pa_name test.physical_addresses in
    let initial_mappings = StringMap.add va_name pa_name test.initial_mappings in
    setup_page_tables { test with virtual_addresses; physical_addresses; initial_mappings } locations

(* TODO build up instructions lists then reverse at the end *)
let generate_test cycle write_values locations =
  let open Test in
  let test = {
    virtual_addresses = StringSet.empty;
    physical_addresses = StringSet.empty;
    initial_mappings = StringMap.empty;
    threads = [new_thread];
    assertion = [];
  } in
  let test = setup_page_tables test locations in
  let write_values = match cycle with
    | (rel, _)::_ when lhs_type rel = Write -> write_values @ [List.hd write_values]
    | _ -> write_values in
  let rec go test cycle write_values loc_diff proc_diff = match cycle with
    | [] -> test
    | (rel, (pa, va))::cycle -> match rel with
      | ProgramOrder (loc_flag, Write, _) ->
        let thread = List.hd test.threads in
        let (setup, value_reg) = new_reg thread.setup (Constant (Int64.of_int @@ List.hd write_values)) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        let instructions = thread.instructions @ [Store (value_reg, loc_reg)] in
        let threads = { setup; instructions } :: List.tl test.threads in
        go ({ test with threads }) cycle (List.tl write_values) loc_flag Internal
      | ProgramOrder (loc_flag, Read, _) ->
        let thread = List.hd test.threads in
        let (setup, result_to) = new_reg thread.setup (Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        let instructions = thread.instructions @ [Load (result_to, loc_reg)] in
        let threads = { setup; instructions } :: List.tl test.threads in
        go ({ test with threads }) cycle write_values loc_flag Internal
      | FromRead (loc_flag, proc_flag) ->
        let thread = List.hd test.threads in
        let (setup, result_to) = new_reg thread.setup (Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        let instructions = thread.instructions @ [Load (result_to, loc_reg)] in
        let threads = match proc_flag with
          | Internal -> { setup; instructions } :: List.tl test.threads
          | External -> new_thread :: { setup; instructions } :: List.tl test.threads in
        let assertion = RegisterAssertion (List.length test.threads - 1, result_to, Int64.of_int (List.hd write_values - 1)) :: test.assertion in
        go ({ test with threads; assertion }) cycle write_values (loc_flag :> location_flag) proc_flag
      | ReadsFrom (loc_flag, proc_flag) ->
        let thread = List.hd test.threads in
        let (setup, value_reg) = new_reg thread.setup (Constant (Int64.of_int @@ List.hd write_values)) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        let instructions = thread.instructions @ [Store (value_reg, loc_reg)] in
        let (threads, read_to) = match proc_flag with
          | Internal -> ({ setup; instructions } :: List.tl test.threads, IntMap.cardinal setup)
          | External -> (new_thread :: { setup; instructions } :: List.tl test.threads, 0) in
        let assertion = RegisterAssertion (List.length threads - 1, read_to, Int64.of_int (List.hd write_values)) :: test.assertion in
        go ({ test with threads; assertion }) cycle (List.tl write_values) (loc_flag :> location_flag) proc_flag in
  let test = go test cycle write_values `Diff External in
  if List.hd test.threads = new_thread then { test with threads = List.tl test.threads } else test

let test_from_cycle cycle =
  let lhs_types = List.map lhs_type cycle in
  let rhs_types = List.map rhs_type cycle in
  if not (lhs_rhs_types_match lhs_types rhs_types) then raise BadCycle else
  let cycle = apply_first_rotation cycle in
  let locations = locations_from_cycle cycle in
  let cycle = apply_second_rotation (zip cycle locations) in
  let write_values = assign_write_values (List.map fst cycle) in
  generate_test cycle write_values locations
  (* Add write serialisation constraints: condition for 2 writes to some pa, or observer thread for more *)