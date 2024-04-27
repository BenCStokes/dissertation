import System.Cmd (system)
import Control.Monad (forM_, void)

run line =
  let (name, _:_:cycle) = span (/= ':') line in
  system ("cd ~/dissertation; dune exec dissertation -- -stdout -name " ++ name ++ " -cycle " ++ cycle ++ " > ~/dissertation/benthesis/new/" ++ name ++ ".litmus.toml")

main = do
  system "mkdir ~/dissertation/benthesis/new"
  cycles <- lines <$> readFile "cycles"
  forM_ cycles $ \line -> case line of
    '#':_ -> return ()
    _ -> void (run line)
  system "cd ~/dissertation/benthesis; diff -ur checked new"
  system "rm -r ~/dissertation/benthesis/new"