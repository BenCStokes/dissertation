type register = int

type arm_barrier_param = SY

let parse_barrier_param = function
  | "SY" -> SY
  | p -> raise (Invalid_argument ("Invalid barrier parameter: " ^ p))

type t = Load of register * register
       | Store of register * register
       | ArmDMBEquiv of arm_barrier_param
       | ArmDSBEquiv of arm_barrier_param
       | ArmISBEquiv
       | ControlDependency of register
       | DataDependency of register * register
       | AddressDependency of register * register
       (* ... *)