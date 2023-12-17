type location_flag = [`Same | `Diff | `Aliased]
type same_pa_location_flag = [`Same | `Aliased]

type event_type_flag = Read | Write

type processor_flag = Internal | External

type relation = ProgramOrder of location_flag * event_type_flag * event_type_flag
              | FromRead of same_pa_location_flag * processor_flag
              (* ... *)

let lhs_type = function
  | ProgramOrder (_, lhs_type, _) -> lhs_type
  | FromRead _ -> Read

let rhs_type = function
  | ProgramOrder (_, _, rhs_type) -> rhs_type
  | FromRead _ -> Write

let processor_info = function
  | ProgramOrder (_, _, _) -> Internal
  | FromRead (_, info) -> info

let location_info = function
  | ProgramOrder (info, _, _) -> info
  | FromRead (info, _) -> (info :> location_flag)

type t = relation list