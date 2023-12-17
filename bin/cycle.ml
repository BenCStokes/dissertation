type location_flag = Same
                   | Diff
                   | Aliased

type event_type_flag = Read | Write

type processor_flag = Internal | External

type relation = ProgramOrder of location_flag * event_type_flag * event_type_flag
              | FromRead of processor_flag
              (* ... *)

let lhs_type = function
  | ProgramOrder (_, lhs_type, _) -> lhs_type
  | FromRead _ -> Read

let rhs_type = function
  | ProgramOrder (_, _, rhs_type) -> rhs_type
  | FromRead _ -> Write

