open Cycle
open Gen

(*let cycle = [FromRead (`Aliased, Internal); ProgramOrder (`Aliased, Write, Read)]*)
let cycle = [FromRead (`Aliased, External); ReadsFrom (`Same, External); ProgramOrder (`Aliased, Read, Read)]

let test = test_from_cycle cycle

module TestPrinter = Test.Printer(AArch64)
let () = Format.printf "%a\n" TestPrinter.pp_test test