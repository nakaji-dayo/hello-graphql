{-# LANGUAGE LambdaCase #-}
module Main where

import MorpheusDemo as M
import HaxlDemo as H
import System.Environment

main :: IO ()
main = do
  getArgs >>= \case
    "haxl":_ -> H.serve2
    _ -> M.serve3
