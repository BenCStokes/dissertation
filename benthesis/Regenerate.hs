import System.Cmd (system)
import Control.Monad (forM_, void)

replace "" = ""
replace ('(':s) = '\\':'(':replace s
replace (')':s) = '\\':')':replace s
replace (c:s) = c:replace s

run line =
  let (name, _:_:cycle') = span (/= ':') line in
  let cycle = replace cycle' in
  system ("cd ~/dissertation; dune exec genonce -- -stdout -name " ++ name ++ " " ++ cycle ++ " > ~/dissertation/benthesis/new/" ++ name ++ ".litmus.toml")
  --system ("cd ~/dissertation; { /usr/bin/time -f \"%E\" ./_build/install/default/bin/dissertation -stdout -name " ++ name ++ " -cycle " ++ cycle ++ " >> ./benthesis/alloutput; } 2>> ./benthesis/benchmark")

main = do
  system "mkdir ~/dissertation/benthesis/new"
  cycles <- lines <$> readFile "cycles"
  forM_ cycles $ \line -> case line of
    '#':_ -> return ()
    _ -> void (run line)
  system "cd ~/dissertation/benthesis; diff -r checked new"
  --system "rm -r ~/dissertation/benthesis/new"