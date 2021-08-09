module Haxl where


import           Haxl.Core
htest = do
  env <- emptyEnv ()
  runHaxl env (pure 42)
