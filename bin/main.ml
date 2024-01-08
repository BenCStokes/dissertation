open Cycle
open Gen

(*let cycle = [FromRead (`Aliased, Internal); ProgramOrder (`Aliased, Write, Read)]*)
let cycle = [FromRead (`Aliased, External); ReadsFrom (`Same, External); ProgramOrder (`Aliased, Read, Read)]

let test = test_from_cycle cycle

let () = let open Test in
  List.iter (fun thread -> print_endline "Thread";
    IntMap.iter (fun reg value -> Format.printf "X%d = %a\n" reg pp_initial_value value) thread.setup;
    List.iter (fun instr -> Format.printf "%a\n" AArch64.print_asm (AArch64.to_concrete_instruction instr)) thread.instructions)
            test.threads;
  Format.printf "Assertion: %a\n" pp_assertion test.assertion

(*let () = let open Format in let open AArch64 in printf "%a" print_asm (LDR(Register 0,Register 1))*)