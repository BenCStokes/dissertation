open Cycle

exception BadCycle of string

let lhs_rhs_types_match lhs_types rhs_types = match (lhs_types, rhs_types) with
  | (first_lhs::lhs_types, rhs_types) ->
    let rec go lhs_types rhs_types = begin match (lhs_types, rhs_types) with
      | ([], [last_rhs]) -> last_rhs = first_lhs
      | (l::lhs_types, r::rhs_types) -> l = r && go lhs_types rhs_types
      | _ -> failwith "lists expected to match in length" end in
    go lhs_types rhs_types
  | _ -> raise (BadCycle "empty cycle")

(*let next_location_name = function
  | 'z' -> failwith "End of alphabet reached (go add more names)"
  | letter -> Char.chr (1 + Char.code letter)*)

let next_location (pa_id, alias_index) relation = match location_info relation with
  | `Same -> (pa_id, alias_index)
  | `Diff -> (pa_id + 1, 0)
  | `Aliased -> (pa_id, alias_index + 1)

let rotate n xs =
  let rec go n = function
    | [] -> if n = 0 then ([], []) else raise (Invalid_argument "Rotated too far")
    | (x::xs) ->
      if n = 0 then
        ((x::xs), [])
      else
        let (l, r) = go (n - 1) xs in (l, x::r) in
  let (l, r) = go n xs in
  l @ r

let rec last = function
  | [] -> raise (Invalid_argument "Last element of empty list")
  | [x] -> x
  | _::xs -> last xs

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

let apply_second_rotation cycle = (* start on a new processor, if possible *)
  let rec find_rotation_point index = function
    | [] -> 0
    | (rel, _)::_ when processor_info rel = External -> index
    | _::cycle -> find_rotation_point (index + 1) cycle in
  let rotation_point = find_rotation_point 1 cycle in
  let rotation_point = if rotation_point = 0 then (* only one processor: don't end on po *)
    let rec count_pos_at_end count = function
      | [] -> count
      | (ProgramOrder (_, _, _), _)::cycle -> count_pos_at_end (count+1) cycle
      | _::cycle -> count_pos_at_end 0 cycle in
    List.length cycle - count_pos_at_end 0 cycle
  else
    rotation_point in
  let cycle = rotate rotation_point cycle in
  (*let rec find_better_rotation_point index = function
    | [] -> 0
    | (rel, _)::_ when processor_info rel = External && location_info rel = `Diff -> index
    | _::cycle -> find_better_rotation_point (index + 1) cycle in
  rotate (find_better_rotation_point 1 cycle)*) cycle

let irange a b =
  let rec go a b xs =
    if a = b then
      xs
    else
      go a (b-1) ((b-1)::xs) in
  go a b []

let get_at_pte cycle =
  let rec count_same_at_end count = function
    | [] -> count
    | (FromRead _ | ReadsFrom _)::cycle -> count_same_at_end (count+1) cycle
    | _::cycle -> count_same_at_end 0 cycle in
  let rotate_by = count_same_at_end 0 cycle in
  let cycle = rotate (List.length cycle - rotate_by) cycle in
  let module IntMap = Map.Make(Int) in
  let from_rhs = rhs_must_be_at_pte (last cycle) in
  let rec go index true_index from_rhs index_to_true_index true_index_to_at_pte = function
    | [] -> List.map (fun i -> IntMap.find (IntMap.find i index_to_true_index) true_index_to_at_pte) (irange 0 index)
    | rel::cycle ->
      let index_to_true_index = IntMap.add index true_index index_to_true_index in
      let true_index_to_at_pte =
        if from_rhs || lhs_must_be_at_pte rel then
          IntMap.add true_index true true_index_to_at_pte
        else
          IntMap.update true_index (function | None -> Some false | v -> v) true_index_to_at_pte in
      let from_rhs = rhs_must_be_at_pte rel in
      let true_index = begin match rel with
        | FromRead _ | ReadsFrom _ | WriteSerialisation _ -> true_index
        | _ -> true_index + 1
      end in
      go (index + 1) true_index from_rhs index_to_true_index true_index_to_at_pte cycle in
  rotate rotate_by @@ go 0 0 from_rhs IntMap.empty IntMap.empty cycle
  (* let from_lhss = List.map lhs_must_be_at_pte cycle in *)
  (* let from_rhss = rotate (List.length cycle - 1) (List.map rhs_must_be_at_pte cycle) in *)
  (* List.map2 (||) from_lhss from_rhss *)

let locations_from_cycle =
  let rec go initial = function
    | [] -> []
    | (rel::cycle) -> initial :: go (next_location initial rel) cycle in
  go (0, 0)

let assign_write_values at_pte cycle =
  let rec count_ws_at_end count = function
    | [] -> count
    | (WriteSerialisation _)::cycle -> count_ws_at_end (count+1) cycle
    | _::cycle -> count_ws_at_end 0 cycle in
  let rotate_by = count_ws_at_end 0 cycle in
  let cycle = List.combine cycle at_pte in
  let cycle = rotate (List.length cycle - rotate_by) cycle in
  let rec go cycle ws = match cycle with
    | [] -> []
    | (rel, at_pte)::cycle -> match rel with
      | ProgramOrder (loc_flag, Write, _)
      | Barrier (loc_flag, Write, _, _) when not at_pte -> ws :: go cycle (if loc_flag = `Diff then 1 else ws + 1)
      | ReadsFrom _ | WriteSerialisation _ when not at_pte -> ws :: go cycle (ws + 1)
      | _ -> go cycle (if location_info rel = `Diff then 1 else ws) in
  rotate rotate_by (go cycle 1)

let assign_translation_write_flags at_pte cycle =
  let rec go cycle flag = match cycle with
    | [] -> []
    | (rel, at_pte)::cycle -> match rel with
      | TranslationFromRead (_, translation_write_flag) -> go cycle (Some translation_write_flag)
      | TranslationReadsFrom (_, translation_write_flag) ->
        if Option.is_some flag && flag <> Some translation_write_flag then
          failwith "Conflicting translation write flags"
        else
          translation_write_flag :: go cycle None
      | ProgramOrder (_, Write, _) | Barrier (_, Write, _, _) | ReadsFrom _ when at_pte ->
        begin match flag with
          | None -> failwith "Failed to assign translation write flag"
          | Some flag -> flag :: go cycle None
        end
      | _ -> go cycle None in
  let flag = match last cycle with | TranslationFromRead (_, translation_write_flag) -> Some translation_write_flag | _ -> None in
  go (List.combine cycle at_pte) flag

let find_maybe_faults cycle =
  let module IntMap = Map.Make(Int) in
  let rec go thread with_multiple = function
    | [_] -> ([], with_multiple)
    | [] -> failwith "This should be unreachable"
    | (TranslationReadsFrom (processor_flag, _))::cycle ->
      let thread = if processor_flag = Internal then thread else 1 + thread in
      let with_multiple = IntMap.update thread (function | None -> Some false | _ -> Some true) with_multiple in
      let (rest, with_multiple) = go thread with_multiple cycle in
      ((true, IntMap.find thread with_multiple)::rest, with_multiple)
    | r::(TranslationFromRead (processor_flag, x))::cycle ->
      let thread = if processor_info r = Internal then thread else 1 + thread in
      let with_multiple = IntMap.update thread (function | None -> Some false | _ -> Some true) with_multiple in
      let (rest, with_multiple) = go thread with_multiple ((TranslationFromRead (processor_flag, x))::cycle) in
      ((true, IntMap.find thread with_multiple)::rest, with_multiple)
    | r::cycle ->
      let thread = if processor_info r = Internal then thread else 1 + thread in
      let (rest, with_multiple) = go thread with_multiple cycle in
      ((false, false)::rest, with_multiple) in
  match (last cycle, List.hd cycle) with
    | (TranslationReadsFrom _, _) | (_, TranslationFromRead _) ->
      let (rest, with_multiple) = go 0 (IntMap.singleton 0 false) cycle in
      (true, IntMap.find 0 with_multiple)::rest
    | _ ->
      let (rest, _) = go 0 IntMap.empty cycle in
      (false, false)::rest

let peek_new_reg setup =
  let module IntMap = Map.Make(Int) in
  let new_key = IntMap.cardinal setup in
  if new_key >= 13 then new_key + 2 else new_key (* X13 and X14 reserved *)

let peek_2nd_reg setup =
  let n = peek_new_reg setup in
  if n = 12 then 15 else 1 + n

let new_reg setup value =
  let new_key = peek_new_reg setup in
  let module IntMap = Map.Make(Int) in
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

let modify_last f = function
  | [] -> raise (Invalid_argument "modify_last of empty list")
  | xs -> match List.rev xs with | (x::xs) -> List.rev (f x::xs) | [] -> failwith "This should be impossible"

let insert_read_and_handler result_to loc_reg thread thread_id (maybe_fault, multiple) =
  let open Test in
  if maybe_fault then
    let handler = if thread.handler = None then
      let handler_reg = if multiple then 14 else result_to in
      Some (0x1000 * (thread_id + 1) + 0x400, Printf.sprintf "    MOV X%d,#0xFFFFFFFFFFFFFFFF\n\n    MRS X13,ELR_EL1\n    ADD X13,X13,#4\n    MSR ELR_EL1,X13\n    ERET\n" handler_reg)
    else
      thread.handler in
    let instructions = thread.instructions @ if multiple then
      [Load (14, loc_reg); Move (result_to, 14)]
    else
      [Load (result_to, loc_reg)] in
    { thread with instructions; handler }
  else
    { thread with instructions = thread.instructions @ [Load (result_to, loc_reg)] }

let cons_or_new x = function
  | None -> Some [x]
  | Some xs -> Some (x::xs)

let simplify_assertion test =
  let open Test in
  let assertion = test.assertion in
  let module StringMap = Map.Make(String) in
  let add_condition map = function
    | RegisterAssertion (proc, reg, v) -> StringMap.update (Format.sprintf "%d %d" proc reg) (cons_or_new (v, true)) map
    | LocationAssertion _ -> map
    | AssertionContradiction -> map
    | NegatedRegisterAssertion (proc, reg, v) -> StringMap.update (Format.sprintf "%d %d" proc reg) (cons_or_new (v, false)) map in
  let map = List.fold_left add_condition StringMap.empty assertion in
  let simp proc reg clauses =
    let module State = struct
      type t = Eq of AssertionValue.t | Neq of AssertionValue.t list | Contradiction
    end in
    let open State in
    let rec go state = function
      | [] -> state
      | (v, false)::clauses -> begin match state with
        | Eq e when AssertionValue.equal e v -> Contradiction
        | Eq e -> go (Eq e) clauses
        | Neq vs when List.exists (AssertionValue.equal v) vs -> go (Neq vs) clauses
        | Neq vs -> go (Neq (v::vs)) clauses
        | Contradiction -> Contradiction end
      | (v, true)::clauses -> begin match state with
        | Eq e when AssertionValue.equal e v -> go (Eq v) clauses
        | Eq _ -> Contradiction
        | Neq vs when List.exists (AssertionValue.equal v) vs -> Contradiction
        | Neq _ -> go (Eq v) clauses
        | Contradiction -> Contradiction end in
    match go (Neq []) clauses with
      | Eq v -> [RegisterAssertion (proc, reg, v)]
      | Neq vs -> List.map (fun v -> NegatedRegisterAssertion (proc, reg, v)) vs
      | Contradiction -> [AssertionContradiction] in
  let assertion = List.filter (function | LocationAssertion _ | AssertionContradiction -> true | _ -> false) assertion in
  let accum str clauses assertion =
    let space = String.index str ' ' in
    let proc = int_of_string (String.sub str 0 space) in
    let reg = int_of_string (String.sub str (space + 1) (String.length str - space - 1)) in
    assertion @ simp proc reg clauses in
  let assertion = StringMap.fold accum map assertion in
  let assertion = if List.exists ((=) AssertionContradiction) assertion then [AssertionContradiction] else assertion in
  { test with assertion }

(* TODO build up instructions lists then reverse at the end *)
(* TODO refactor all the repeated code here *)
let generate_test at_pte name orig cycle write_values translation_write_flags locations maybe_faults =
  let open Test in
  let test = {
    name = name;
    cycle = orig;
    virtual_addresses = StringSet.empty;
    physical_addresses = StringSet.empty;
    initial_mappings = StringMap.empty;
    possible_mappings = StringMap.empty;
    threads = [new_thread];
    assertion = [];
  } in
  let test = setup_page_tables test locations in
  let write_values = match cycle with
    | (rel, _)::_ when lhs_type rel = Write && not (List.hd at_pte) -> write_values @ [List.hd write_values]
    | _ -> write_values in
  let cycle = List.map2 (fun (rel, loc) at_pte -> (rel, loc, at_pte)) cycle at_pte in
  let cycle = List.map2 (fun (rel, loc, at_pte) maybe_fault -> (rel, loc, at_pte, maybe_fault)) cycle maybe_faults in
  (*let whole_cycle = cycle in*)
  let rec go test cycle write_values translation_write_flags loc_diff proc_diff writes_by_location = match cycle with
    | [] -> (test, writes_by_location)
    | (rel, (pa, va), at_pte, maybe_fault)::cycle -> match rel with
      | ProgramOrder (loc_flag, Write, _) ->
        if at_pte then
          let thread = List.hd test.threads in
          let pa_name = Printf.sprintf "pa_%d" pa in
          let (setup, value_reg) = new_reg thread.setup (match List.hd translation_write_flags with | Make -> MkDesc pa_name | Break -> Constant 0L) in
          let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
          let (setup, loc_reg) = new_reg setup (PTE va_name) in
          let instructions = thread.instructions @ [Store (value_reg, loc_reg)] in
          let threads = { thread with setup; instructions } :: List.tl test.threads in
          go ({ test with threads }) cycle write_values (List.tl translation_write_flags) `Diff Internal writes_by_location
        else
          let thread = List.hd test.threads in
          let (setup, value_reg) = new_reg thread.setup (Constant (Int64.of_int @@ List.hd write_values)) in
          let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
          let (setup, loc_reg) = if loc_diff = `Same && proc_diff = Internal then
            (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
          else
            new_reg setup (VirtualAddress va_name) in
          let instructions = thread.instructions @ [Store (value_reg, loc_reg)] in
          let threads = { thread with setup; instructions } :: List.tl test.threads in
          let writes_by_location = IntMap.update pa (function | None -> Some (List.hd write_values) | Some n -> Some (max n (List.hd write_values))) writes_by_location in
          go ({ test with threads }) cycle (List.tl write_values) translation_write_flags loc_flag Internal writes_by_location
      | ProgramOrder (_, TLBI op, _) ->
        let thread = List.hd test.threads in
        (* let (setup, result_to) = new_reg thread.setup (Constant 0L) in *)
        let setup = thread.setup in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if at_pte then
          failwith "TLBI at PTE"
        else (*if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else*)
          new_reg setup (Page va_name) in
        let instructions = thread.instructions @ [ArmTLBIEquiv (op, loc_reg)] in
        let threads = { thread with setup; instructions; el1 = true } :: List.tl test.threads in
        go ({ test with threads }) cycle write_values translation_write_flags `Diff Internal writes_by_location
      | Barrier (loc_flag, Write, _, specs) ->
        if at_pte then
          let thread = List.hd test.threads in
          let pa_name = Printf.sprintf "pa_%d" pa in
          let (setup, value_reg) = new_reg thread.setup (match List.hd translation_write_flags with | Make -> MkDesc pa_name | Break -> Constant 0L) in
          let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
          let (setup, loc_reg) = new_reg setup (PTE va_name) in
          let instructions = thread.instructions @ Store (value_reg, loc_reg) :: specs in
          let threads = { thread with setup; instructions } :: List.tl test.threads in
          go ({ test with threads }) cycle write_values (List.tl translation_write_flags) `Diff Internal writes_by_location
        else
          let thread = List.hd test.threads in
          let (setup, value_reg) = new_reg thread.setup (Constant (Int64.of_int @@ List.hd write_values)) in
          let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
          let (setup, loc_reg) = if loc_diff = `Same && proc_diff = Internal then
            (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
          else
            new_reg setup (VirtualAddress va_name) in
          let instructions = thread.instructions @ Store (value_reg, loc_reg) :: specs in
          let threads = { thread with setup; instructions } :: List.tl test.threads in
          let writes_by_location = IntMap.update pa (function | None -> Some (List.hd write_values) | Some n -> Some (max n (List.hd write_values))) writes_by_location in
          go ({ test with threads }) cycle (List.tl write_values) translation_write_flags loc_flag Internal writes_by_location
      | ProgramOrder (loc_flag, Read, _) ->
        let thread = List.hd test.threads in
        let (setup, result_to) = new_reg thread.setup (Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if at_pte then
          new_reg setup (PTE va_name)
        else if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        (* let instructions = thread.instructions @ [Load (result_to, loc_reg)] in *)
        let thread = insert_read_and_handler result_to loc_reg thread (List.length test.threads - 1) maybe_fault in
        let threads = { thread with setup } :: List.tl test.threads in
        go ({ test with threads }) cycle write_values translation_write_flags (if at_pte then `Diff else loc_flag) Internal writes_by_location
      | Barrier (loc_flag, Read, _, specs) ->
        let thread = List.hd test.threads in
        let (setup, result_to) = new_reg thread.setup (Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if at_pte then
          new_reg setup (PTE va_name)
        else if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        (* let instructions = thread.instructions @ Load (result_to, loc_reg) :: specs in *)
        let thread = insert_read_and_handler result_to loc_reg thread (List.length test.threads - 1) maybe_fault in
        let instructions = thread.instructions @ specs in
        let threads = { thread with setup; instructions } :: List.tl test.threads in
        go ({ test with threads }) cycle write_values translation_write_flags (if at_pte then `Diff else loc_flag) Internal writes_by_location
      | Barrier (_, TLBI op, _, specs) ->
        let thread = List.hd test.threads in
        (* let (setup, result_to) = new_reg thread.setup (Constant 0L) in *)
        let setup = thread.setup in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if at_pte then
          failwith "TLBI at PTE"
        else (*if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else*)
          new_reg setup (Page va_name) in
        let instructions = thread.instructions @ ArmTLBIEquiv (op, loc_reg) :: specs in
        let threads = { thread with setup; instructions; el1 = true } :: List.tl test.threads in
        go ({ test with threads }) cycle write_values translation_write_flags `Diff Internal writes_by_location
      | Dependency (dependency_type, loc_flag, _) ->
        let thread = List.hd test.threads in
        let (setup, result_to) = new_reg thread.setup (Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if at_pte then
          new_reg setup (PTE va_name)
        else if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        let dependency_instructions = match dependency_type with
          | Control -> [Instruction.ControlDependency result_to]
          | Data -> [Instruction.DataDependency (result_to, peek_new_reg setup)]
          | Address -> [Instruction.AddressDependency (result_to, peek_2nd_reg setup)] in (* FIXME wrong address sometimes *) (* wait... is it? *)
        (* let instructions = thread.instructions @ Load (result_to, loc_reg) :: dependency_instructions in *)
        let thread = insert_read_and_handler result_to loc_reg thread (List.length test.threads - 1) maybe_fault in
        let instructions = thread.instructions @ dependency_instructions in
        let threads = { thread with setup; instructions } :: List.tl test.threads in
        go ({ test with threads }) cycle write_values translation_write_flags (if at_pte then `Diff else loc_flag) Internal writes_by_location
      (* | ProgramOrder (_, TranslationWrite, _) ->
        let thread = List.hd test.threads in
        let pa_name = Printf.sprintf "pa_%d" pa in
        let (setup, value_reg) = new_reg thread.setup (match List.hd translation_write_flags with | Make -> MkDesc pa_name | Break -> Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = new_reg setup (PTE va_name) in
        let instructions = thread.instructions @ [Store (value_reg, loc_reg)] in
        let threads = { thread with setup; instructions } :: List.tl test.threads in
        go ({ test with threads }) cycle write_values (List.tl translation_write_flags) `Diff Internal writes_by_location *)
      (* | Barrier (_, TranslationWrite, _, specs) ->
        let thread = List.hd test.threads in
        let pa_name = Printf.sprintf "pa_%d" pa in
        let (setup, value_reg) = new_reg thread.setup (match List.hd translation_write_flags with | Make -> MkDesc pa_name | Break -> Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = new_reg setup (PTE va_name) in
        let instructions = thread.instructions @ Store (value_reg, loc_reg) :: specs in
        let threads = { thread with setup; instructions } :: List.tl test.threads in
        go ({ test with threads }) cycle write_values (List.tl translation_write_flags) `Diff Internal writes_by_location *)
      | FromRead (loc_flag, proc_flag) ->
        let thread = List.hd test.threads in
        let (setup, result_to) = new_reg thread.setup (Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if at_pte then
          new_reg setup (PTE va_name)
        else if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        (* let instructions = thread.instructions @ [Load (result_to, loc_reg)] in *)
        let thread = insert_read_and_handler result_to loc_reg thread (List.length test.threads - 1) maybe_fault in
        let threads = match proc_flag with
          | Internal -> { thread with setup } :: List.tl test.threads
          | External -> new_thread :: { thread with setup } :: List.tl test.threads in
        let assertion = if at_pte then
          let pa_name = Printf.sprintf "pa_%d" pa in
          RegisterAssertion (List.length test.threads - 1, result_to, match List.hd translation_write_flags with | Break -> AssertionValue.PTE pa_name | Make -> AssertionValue.Constant 0L) :: test.assertion
        else
          RegisterAssertion (List.length test.threads - 1, result_to, AssertionValue.Constant (Int64.of_int (List.hd write_values - 1))) :: test.assertion in
        go ({ test with threads; assertion }) cycle write_values translation_write_flags (if at_pte then `Diff else (loc_flag :> location_flag)) proc_flag writes_by_location
      | ReadsFrom (loc_flag, proc_flag) ->
        let thread = List.hd test.threads in
        let pa_name = Printf.sprintf "pa_%d" pa in
        let (setup, value_reg) = new_reg thread.setup (if at_pte then (match List.hd translation_write_flags with | Make -> MkDesc pa_name | Break -> Constant 0L) else Constant (Int64.of_int @@ List.hd write_values)) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if at_pte then
          new_reg setup (PTE va_name)
        else if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        let instructions = thread.instructions @ [Store (value_reg, loc_reg)] in
        let (threads, read_to) = match proc_flag with
          | Internal -> ({ thread with setup; instructions } :: List.tl test.threads, peek_new_reg setup)
          | External -> (new_thread :: { thread with setup; instructions } :: List.tl test.threads, 0) in
        let thread_num = if cycle = [] && proc_flag = External then 0 else List.length threads - 1 in
        if at_pte then
          let assertion = RegisterAssertion (thread_num, read_to, match List.hd translation_write_flags with | Make -> AssertionValue.PTE pa_name | Break -> AssertionValue.Constant 0L) :: test.assertion in
          go ({ test with threads; assertion }) cycle write_values (List.tl translation_write_flags) `Diff proc_flag writes_by_location
        else
          let assertion = RegisterAssertion (thread_num, read_to, AssertionValue.Constant (Int64.of_int (List.hd write_values))) :: test.assertion in
          let writes_by_location = IntMap.update pa (function | None -> Some (List.hd write_values) | Some n -> Some (max n (List.hd write_values))) writes_by_location in
          go ({ test with threads; assertion }) cycle (List.tl write_values) translation_write_flags (loc_flag :> location_flag) proc_flag writes_by_location
      | WriteSerialisation (loc_flag, proc_flag) -> 
        let thread = List.hd test.threads in
        let (setup, value_reg) = new_reg thread.setup (Constant (Int64.of_int @@ List.hd write_values)) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let (setup, loc_reg) = if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        let instructions = thread.instructions @ [Store (value_reg, loc_reg)] in
        let threads = match proc_flag with
          | Internal -> { thread with setup; instructions } :: List.tl test.threads
          | External -> new_thread :: { thread with setup; instructions } :: List.tl test.threads in
        let writes_by_location = IntMap.update pa (function | None -> Some (List.hd write_values) | Some n -> Some (max n (List.hd write_values))) writes_by_location in
        go ({ test with threads }) cycle (List.tl write_values) translation_write_flags (loc_flag :> location_flag) proc_flag writes_by_location
      | TranslationReadsFrom (proc_flag, translation_write_flag) ->
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let pa_name = Printf.sprintf "pa_%d" pa in
        let (before_mapping, after_mapping) = match translation_write_flag with
          | Make -> ("invalid", pa_name)
          | Break -> (pa_name, "invalid") in
        let initial_mappings = StringMap.add va_name before_mapping test.initial_mappings in
        let possible_mappings = StringMap.add va_name after_mapping test.possible_mappings in
        let thread = List.hd test.threads in
        let (setup, value_reg) = new_reg thread.setup (match translation_write_flag with | Make -> MkDesc pa_name | Break -> Constant 0L) in
        let (setup, loc_reg) = new_reg setup (PTE va_name) in
        let read_result_reg = match (cycle, proc_flag) with | ([], _) | (_, External) -> 0 | (_, Internal) -> peek_new_reg setup in
        (* let read_result_reg = match cycle with | [] -> 0 | _ -> match proc_flag with | Internal -> peek_new_reg setup | External -> 0 in *)
        (* let thread_id = match proc_flag with | Internal -> List.length test.threads - 1 | External -> List.length test.threads in *)
        (* let handler = Some (0x1000 * (thread_id + 1) + 0x400, Printf.sprintf "    MOV X%d,#1\n\n    MRS X13,ELR_EL1\n    ADD X13,X13,#4\n    MSR ELR_EL1,X13\n    ERET\n" read_result_reg) in *)
        let instructions = thread.instructions @ [Store (value_reg, loc_reg)] in
        let threads = match proc_flag with
          | Internal -> { thread with setup; instructions } :: List.tl test.threads
          | External -> new_thread :: { thread with setup; instructions } :: List.tl test.threads in
          (* let writes_by_location = IntMap.update pa (function | None -> Some (List.hd write_values) | Some n -> Some (max n (List.hd write_values))) writes_by_location in *)
        let thread_num = if cycle = [] && proc_flag = External then 0 else List.length threads - 1 in
        let condition = match translation_write_flag with
          | Make -> NegatedRegisterAssertion (thread_num, read_result_reg, AssertionValue.FaultIndicator)
          | Break -> RegisterAssertion (thread_num, read_result_reg, AssertionValue.FaultIndicator) in
        let assertion = condition :: test.assertion in
        go ({ test with threads; initial_mappings; possible_mappings; assertion }) cycle write_values (List.tl translation_write_flags) `Diff proc_flag writes_by_location
      | TranslationFromRead (proc_flag, translation_write_flag) ->
        let thread = List.hd test.threads in
        let (setup, result_to) = new_reg thread.setup (Constant 0L) in
        let va_name = Printf.sprintf "pa_%d_va_%d" pa va in
        let pa_name = Printf.sprintf "pa_%d" pa in
        let (before_mapping, after_mapping) = match translation_write_flag with
          | Make -> ("invalid", pa_name)
          | Break -> (pa_name, "invalid") in
        let initial_mappings = StringMap.add va_name before_mapping test.initial_mappings in
        let possible_mappings = StringMap.add va_name after_mapping test.possible_mappings in
        let (setup, loc_reg) = if loc_diff = `Same && proc_diff = Internal then
          (setup, fst (List.find (fun (_, v) -> v = VirtualAddress va_name) (IntMap.bindings setup)))
        else
          new_reg setup (VirtualAddress va_name) in
        let thread_id = List.length test.threads - 1 in
        (* let handler = Some (0x1000 * (thread_id + 1) + 0x400, Printf.sprintf "    MOV X%d,#1\n\n    MRS X13,ELR_EL1\n    ADD X13,X13,#4\n    MSR ELR_EL1,X13\n    ERET\n" result_to) in *)
        (* let instructions = thread.instructions @ [Load (result_to, loc_reg)] in *)
        let thread = insert_read_and_handler result_to loc_reg thread (List.length test.threads - 1) maybe_fault in
        let threads = match proc_flag with
          | Internal -> { thread with setup } :: List.tl test.threads
          | External -> new_thread :: { thread with setup } :: List.tl test.threads in
        let condition = match translation_write_flag with
          | Break -> NegatedRegisterAssertion (thread_id, result_to, AssertionValue.FaultIndicator)
          | Make -> RegisterAssertion (thread_id, result_to, AssertionValue.FaultIndicator) in
        let assertion = condition :: test.assertion in
        go ({ test with threads; assertion; initial_mappings; possible_mappings }) cycle write_values translation_write_flags `Same proc_flag writes_by_location in
  let (test, writes_by_location) = go test cycle write_values translation_write_flags `Diff External IntMap.empty in
  let test = if List.hd test.threads = new_thread then { test with threads = List.tl test.threads } else test in
  (* let test = if { (List.hd test.threads) with handler = None } = new_thread then
    { test with threads = modify_last (fun t -> if Option.is_some t.handler then failwith "Multiple handlers on first threads" else { t with handler = Option.map (fun (_, h) -> (0x1400, h)) (List.hd test.threads).handler } ) (List.tl test.threads) }
  else test in *)
  let add_ws_assertion pa writes test =
    let pa_name = "pa_" ^ string_of_int pa in
    if writes < 2 then
      test
    else if writes = 2 then
      let assertion = LocationAssertion (pa_name, Int64.of_int 2) :: test.assertion in
      { test with assertion }
    else
      let va_name = Format.sprintf "pa_%d_observer" pa in
      let rec make_instructions i instructions =
        let instructions = Instruction.Load (i, 0) :: instructions in
        if i = 1 then instructions else make_instructions (i - 1) instructions in
      let observer = { el1 = false; setup = IntMap.singleton 0 (VirtualAddress va_name); instructions = make_instructions writes []; handler = None; } in
      let thread_id = List.length test.threads in
      { test with
          threads = observer :: test.threads;
          virtual_addresses = StringSet.add va_name test.virtual_addresses;
          initial_mappings = StringMap.add va_name pa_name test.initial_mappings;
          assertion = test.assertion @ List.init writes (fun i -> RegisterAssertion (thread_id, i + 1, AssertionValue.Constant (i + 1 |> Int64.of_int)))
      } in
  let test = IntMap.fold add_ws_assertion writes_by_location test in
  simplify_assertion test

let test_from_cycle name orig cycle =
  let lhs_types = List.map lhs_type cycle in
  let rhs_types = List.map rhs_type cycle in
  if not (lhs_rhs_types_match lhs_types rhs_types) then raise (BadCycle "non-matching types") else
  let cycle = apply_first_rotation cycle in
  let locations = locations_from_cycle cycle in
  let cycle = apply_second_rotation (List.combine cycle locations) in
  let at_pte = get_at_pte (List.map fst cycle) in
  let write_values = assign_write_values at_pte (List.map fst cycle) in
  let translation_write_flags = assign_translation_write_flags at_pte (List.map fst cycle) in
  let maybe_faults = find_maybe_faults (List.map fst cycle) in
  generate_test at_pte name orig cycle write_values translation_write_flags locations maybe_faults