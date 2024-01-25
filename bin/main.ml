open Gen

(*let cycle =
  (*[FromRead (`Same, Internal); ProgramOrder (`Same, Write, Read)]*) (* CoWR? *)
  (*[FromRead (`Aliased, Internal); ProgramOrder (`Aliased, Write, Read)]*) (* CoWR.alias *)
  (*[FromRead (`Aliased, External); ReadsFrom (`Same, External); ProgramOrder (`Aliased, Read, Read)]*) (* CoRR0.alias+po *)
  (*[FromRead (`Same, External); ReadsFrom (`Same, External); ProgramOrder (`Same, Read, Read)]*) (* CoRR0+po? *)
  (*[ProgramOrder (`Same, Write, Read); FromRead (`Same, External); WriteSerialisation (`Same, External)]*)

let test = test_from_cycle cycle*)

let usage = "testgen [-name <name>] -cycle <cycle>"
let cycle = ref []
let test_name = ref "PLACEHOLDER"
let args = [
  "-name", Arg.Set_string test_name, "The name of the test";
  "-cycle", Arg.Rest_all (fun tokens -> cycle := List.map Cycle.parse_relation tokens), "The cycle to generate the test from";
]
let () = Arg.parse args (fun arg -> print_endline ("Ignored anonymous argument: " ^ arg)) usage

module TestPrinter = Test.Printer(AArch64)
let () =
  let test = test_from_cycle !test_name !cycle in
  Format.printf "%a\n" TestPrinter.pp_test test