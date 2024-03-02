import System.Cmd (system)
import Control.Monad (forM_, void)

run line =
  let (name, _:_:cycle) = span (/= ':') line in
  system ("~/dissertation/_build/install/default/bin/dissertation -stdout -name " ++ name ++ " -cycle " ++ cycle ++ " > ~/dissertation/benthesis/new/" ++ name ++ ".litmus.toml")

main = do
  cycles <- lines <$> readFile "cycles"
  forM_ cycles $ \line -> case line of
    '#':_ -> return ()
    _ -> void (run line)