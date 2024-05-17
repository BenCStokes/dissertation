(* module EventOrd = struct
  type t = Cycle.event_type_flag
  let compare = compare
end

module EventMap = Map.Make(EventOrd)

let gen_cycles safe relaxed size nprocs =
  let update_map x = function | None -> Some [] | Some xs -> Some (x::xs) in
  let by_rhs = List.fold_left (fun map rel -> EventMap.update (Cycle.rhs_type rel) (update_map rel) map) EventMap.empty (relaxed::safe) in
  let cycles = ref [] in
  let dfs cycle depth procs =
    match cycle with | [] -> failwith "Should be impossible" | (last::rest) as cycle ->
    let candidates = EventMap.find *)